// export.cc -- Export declarations in Go frontend.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-sha1.h"
#include "go-c.h"

#include "gogo.h"
#include "types.h"
#include "statements.h"
#include "export.h"

// This file handles exporting global declarations.

// Class Export.

const int Export::magic_len;

// Current version magic string.
const char Export::cur_magic[Export::magic_len] =
  {
    'v', '2', ';', '\n'
  };

// Magic string for previous version (still supported)
const char Export::v1_magic[Export::magic_len] =
  {
    'v', '1', ';', '\n'
  };

const int Export::checksum_len;

// Constructor.

Export::Export(Stream* stream)
  : stream_(stream), type_refs_(), type_index_(1), packages_()
{
  go_assert(Export::checksum_len == Go_sha1_helper::checksum_len);
}

// A functor to sort Named_object pointers by name.

struct Sort_bindings
{
  bool
  operator()(const Named_object* n1, const Named_object* n2) const
  { return n1->name() < n2->name(); }
};

// Return true if we should export NO.

static bool
should_export(Named_object* no)
{
  // We only export objects which are locally defined.
  if (no->package() != NULL)
    return false;

  // We don't export packages.
  if (no->is_package())
    return false;

  // We don't export hidden names.
  if (Gogo::is_hidden_name(no->name()))
    return false;

  // We don't export nested functions.
  if (no->is_function() && no->func_value()->enclosing() != NULL)
    return false;

  // We don't export thunks.
  if (no->is_function() && Gogo::is_thunk(no))
    return false;

  // Methods are exported with the type, not here.
  if (no->is_function()
      && no->func_value()->type()->is_method())
    return false;
  if (no->is_function_declaration()
      && no->func_declaration_value()->type()->is_method())
    return false;

  // Don't export dummy global variables created for initializers when
  // used with sinks.
  if (no->is_variable() && no->name()[0] == '_' && no->name()[1] == '.')
    return false;

  return true;
}

// Export those identifiers marked for exporting.

void
Export::export_globals(const std::string& package_name,
		       const std::string& prefix,
		       const std::string& pkgpath,
		       const std::map<std::string, Package*>& packages,
		       const std::map<std::string, Package*>& imports,
		       const std::string& import_init_fn,
                       const Import_init_set& imported_init_fns,
		       const Bindings* bindings)
{
  // If there have been any errors so far, don't try to export
  // anything.  That way the export code doesn't have to worry about
  // mismatched types or other confusions.
  if (saw_errors())
    return;

  // Export the symbols in sorted order.  That will reduce cases where
  // irrelevant changes to the source code affect the exported
  // interface.
  std::vector<Named_object*> exports;
  exports.reserve(bindings->size_definitions());

  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p)
    if (should_export(*p))
      exports.push_back(*p);

  for (Bindings::const_declarations_iterator p =
	 bindings->begin_declarations();
       p != bindings->end_declarations();
       ++p)
    {
      // We export a function declaration as it may be implemented in
      // supporting C code.  We do not export type declarations.
      if (p->second->is_function_declaration()
	  && should_export(p->second))
	exports.push_back(p->second);
    }

  std::sort(exports.begin(), exports.end(), Sort_bindings());

  // Although the export data is readable, at least this version is,
  // it is conceptually a binary format.  Start with a four byte
  // version number.
  this->write_bytes(Export::cur_magic, Export::magic_len);

  // The package name.
  this->write_c_string("package ");
  this->write_string(package_name);
  this->write_c_string(";\n");

  // The prefix or package path, used for all global symbols.
  if (prefix.empty())
    {
      go_assert(!pkgpath.empty());
      this->write_c_string("pkgpath ");
      this->write_string(pkgpath);
    }
  else
    {
      this->write_c_string("prefix ");
      this->write_string(prefix);
    }
  this->write_c_string(";\n");

  this->write_packages(packages);

  this->write_imports(imports);

  this->write_imported_init_fns(package_name, import_init_fn,
				imported_init_fns);

  // FIXME: It might be clever to add something about the processor
  // and ABI being used, although ideally any problems in that area
  // would be caught by the linker.

  for (std::vector<Named_object*>::const_iterator p = exports.begin();
       p != exports.end();
       ++p)
    (*p)->export_named_object(this);

  std::string checksum = this->stream_->checksum();
  std::string s = "checksum ";
  for (std::string::const_iterator p = checksum.begin();
       p != checksum.end();
       ++p)
    {
      unsigned char c = *p;
      unsigned int dig = c >> 4;
      s += dig < 10 ? '0' + dig : 'A' + dig - 10;
      dig = c & 0xf;
      s += dig < 10 ? '0' + dig : 'A' + dig - 10;
    }
  s += ";\n";
  this->stream_->write_checksum(s);
}

// Sort packages.

static bool
packages_compare(const Package* a, const Package* b)
{
  return a->package_name() < b->package_name();
}

// Write out all the known packages whose pkgpath symbol is not a
// simple transformation of the pkgpath, so that the importing code
// can reliably know it.

void
Export::write_packages(const std::map<std::string, Package*>& packages)
{
  // Sort for consistent output.
  std::vector<Package*> out;
  for (std::map<std::string, Package*>::const_iterator p = packages.begin();
       p != packages.end();
       ++p)
    {
      if (p->second->pkgpath_symbol()
	  != Gogo::pkgpath_for_symbol(p->second->pkgpath()))
	out.push_back(p->second);
    }

  std::sort(out.begin(), out.end(), packages_compare);

  for (std::vector<Package*>::const_iterator p = out.begin();
       p != out.end();
       ++p)
    {
      this->write_c_string("package ");
      this->write_string((*p)->package_name());
      this->write_c_string(" ");
      this->write_string((*p)->pkgpath());
      this->write_c_string(" ");
      this->write_string((*p)->pkgpath_symbol());
      this->write_c_string(";\n");
    }
}

// Sort imported packages.

static bool
import_compare(const std::pair<std::string, Package*>& a,
	       const std::pair<std::string, Package*>& b)
{
  return a.first < b.first;
}

// Write out the imported packages.

void
Export::write_imports(const std::map<std::string, Package*>& imports)
{
  // Sort the imports for more consistent output.
  std::vector<std::pair<std::string, Package*> > sorted_imports;
  for (std::map<std::string, Package*>::const_iterator p = imports.begin();
       p != imports.end();
       ++p)
    sorted_imports.push_back(std::make_pair(p->first, p->second));

  std::sort(sorted_imports.begin(), sorted_imports.end(), import_compare);

  for (std::vector<std::pair<std::string, Package*> >::const_iterator p =
	 sorted_imports.begin();
       p != sorted_imports.end();
       ++p)
    {
      this->write_c_string("import ");
      this->write_string(p->second->package_name());
      this->write_c_string(" ");
      this->write_string(p->second->pkgpath());
      this->write_c_string(" \"");
      this->write_string(p->first);
      this->write_c_string("\";\n");

      this->packages_.insert(p->second);
    }
}

void
Export::add_init_graph_edge(Init_graph* init_graph, unsigned src, unsigned sink)
{
  Init_graph::iterator it = init_graph->find(src);
  if (it != init_graph->end())
    it->second.insert(sink);
  else
    {
      std::set<unsigned> succs;
      succs.insert(sink);
      (*init_graph)[src] = succs;
    }
}

// Constructs the imported portion of the init graph, e.g. those
// edges that we read from imported packages.

void
Export::populate_init_graph(Init_graph* init_graph,
                            const Import_init_set& imported_init_fns,
                            const std::map<std::string, unsigned>& init_idx)
{
  for (Import_init_set::const_iterator p = imported_init_fns.begin();
       p != imported_init_fns.end();
       ++p)
    {
      const Import_init* ii = *p;
      std::map<std::string, unsigned>::const_iterator srcit =
          init_idx.find(ii->init_name());
      go_assert(srcit != init_idx.end());
      unsigned src = srcit->second;
      for (std::set<std::string>::const_iterator pci = ii->precursors().begin();
           pci != ii->precursors().end();
           ++pci)
	{
	  std::map<std::string, unsigned>::const_iterator it =
	      init_idx.find(*pci);
	  go_assert(it != init_idx.end());
	  unsigned sink = it->second;
	  add_init_graph_edge(init_graph, src, sink);
	}
    }
}

// Write out the initialization functions which need to run for this
// package.

void
Export::write_imported_init_fns(const std::string& package_name,
                                const std::string& import_init_fn,
                                const Import_init_set& imported_init_fns)
{
  if (import_init_fn.empty() && imported_init_fns.empty()) return;

  // Maps a given init function to the its index in the exported "init" clause.
  std::map<std::string, unsigned> init_idx;

  this->write_c_string("init");

  if (!import_init_fn.empty())
    {
      this->write_c_string(" ");
      this->write_string(package_name);
      this->write_c_string(" ");
      this->write_string(import_init_fn);
      init_idx[import_init_fn] = 0;
    }

  if (imported_init_fns.empty())
    {
      this->write_c_string(";\n");
      return;
    }

  typedef std::map<int, std::vector<std::string> > level_map;
  Init_graph init_graph;
  level_map inits_at_level;

  // Walk through the set of import inits (already sorted by
  // init fcn name) and write them out to the exports.
  for (Import_init_set::const_iterator p = imported_init_fns.begin();
       p != imported_init_fns.end();
       ++p)
    {
      const Import_init* ii = *p;
      this->write_c_string(" ");
      this->write_string(ii->package_name());
      this->write_c_string(" ");
      this->write_string(ii->init_name());

      // Populate init_idx.
      go_assert(init_idx.find(ii->init_name()) == init_idx.end());
      unsigned idx = init_idx.size();
      init_idx[ii->init_name()] = idx;

      // If the init function has a non-negative priority value, this
      // is an indication that it was referred to in an older version
      // export data section (e.g. we read a legacy object
      // file). Record such init fcns so that we can fix up the graph
      // for them (handled later in this function).
      if (ii->priority() > 0)
	{
	  level_map::iterator it = inits_at_level.find(ii->priority());
	  if (it == inits_at_level.end())
	    {
	      std::vector<std::string> l;
	      l.push_back(ii->init_name());
	      inits_at_level[ii->priority()] = l;
	    }
	  else
	    it->second.push_back(ii->init_name());
	}
    }
  this->write_c_string(";\n");

  // Create the init graph. Start by populating the graph with
  // all the edges we inherited from imported packages.
  populate_init_graph(&init_graph, imported_init_fns, init_idx);

  // Now add edges from the local init function to each of the
  // imported fcns.
  if (!import_init_fn.empty())
    {
      unsigned src = 0;
      go_assert(init_idx[import_init_fn] == 0);
      for (Import_init_set::const_iterator p = imported_init_fns.begin();
           p != imported_init_fns.end();
           ++p)
	{
          const Import_init* ii = *p;
	  unsigned sink = init_idx[ii->init_name()];
	  add_init_graph_edge(&init_graph, src, sink);
	}
    }

  // In the scenario where one or more of the packages we imported
  // was written with the legacy export data format, add dummy edges
  // to capture the priority relationships. Here is a package import
  // graph as an example:
  //
  //       *A
  //       /|
  //      / |
  //     B  *C
  //       /|
  //      / |
  //    *D *E
  //     | /|
  //     |/ |
  //    *F  *G
  //
  // Let's suppose that the object for package "C" is from an old
  // gccgo, e.g. it has the old export data format. All other
  // packages are compiled with the new compiler and have the new
  // format. Packages with *'s have init functions. The scenario is
  // that we're compiling a package "A"; during this process we'll
  // read the export data for "C". It should look something like
  //
  //   init F F..import 1 G G..import 1 D D..import 2 E E..import 2;
  //
  // To capture this information and convey it to the consumers of
  // "A", the code below adds edges to the graph from each priority K
  // function to every priority K-1 function for appropriate values
  // of K. This will potentially add more edges than we need (for
  // example, an edge from D to G), but given that we don't expect
  // to see large numbers of old objects, this will hopefully be OK.

  if (inits_at_level.size() > 0)
    {
      for (level_map::reverse_iterator it = inits_at_level.rbegin();
           it != inits_at_level.rend(); ++it)
	{
	  int level = it->first;
	  if (level < 2) break;
	  const std::vector<std::string>& fcns_at_level = it->second;
	  for (std::vector<std::string>::const_iterator sit =
	           fcns_at_level.begin();
	       sit != fcns_at_level.end(); ++sit)
	    {
	      unsigned src = init_idx[*sit];
	      level_map::iterator it2 = inits_at_level.find(level - 1);
	      if (it2 != inits_at_level.end())
		{
		  const std::vector<std::string> fcns_at_lm1 = it2->second;
		  for (std::vector<std::string>::const_iterator mit =
		           fcns_at_lm1.begin();
		       mit != fcns_at_lm1.end(); ++mit)
		    {
		      unsigned sink = init_idx[*mit];
		      add_init_graph_edge(&init_graph, src, sink);
		    }
		}
	    }
	}
    }

  // Write out the resulting graph.
  this->write_c_string("init_graph");
  for (Init_graph::const_iterator ki = init_graph.begin();
       ki != init_graph.end(); ++ki)
    {
      unsigned src = ki->first;
      const std::set<unsigned>& successors = ki->second;
      for (std::set<unsigned>::const_iterator vi = successors.begin();
           vi != successors.end(); ++vi)
	{
	  this->write_c_string(" ");
	  this->write_unsigned(src);
	  unsigned sink = (*vi);
	  this->write_c_string(" ");
	  this->write_unsigned(sink);
	}
    }
  this->write_c_string(";\n");
}

// Write a name to the export stream.

void
Export::write_name(const std::string& name)
{
  if (name.empty())
    this->write_c_string("?");
  else
    this->write_string(Gogo::message_name(name));
}

// Write an integer value to the export stream.

void
Export::write_int(int value)
{
  char buf[100];
  snprintf(buf, sizeof buf, "%d", value);
  this->write_c_string(buf);
}

// Write an integer value to the export stream.

void
Export::write_unsigned(unsigned value)
{
  char buf[100];
  snprintf(buf, sizeof buf, "%u", value);
  this->write_c_string(buf);
}

// Export a type.  We have to ensure that on import we create a single
// Named_type node for each named type.  We do this by keeping a hash
// table mapping named types to reference numbers.  The first time we
// see a named type we assign it a reference number by making an entry
// in the hash table.  If we see it again, we just refer to the
// reference number.

// Named types are, of course, associated with packages.  Note that we
// may see a named type when importing one package, and then later see
// the same named type when importing a different package.  The home
// package may or may not be imported during this compilation.  The
// reference number scheme has to get this all right.  Basic approach
// taken from "On the Linearization of Graphs and Writing Symbol
// Files" by Robert Griesemer.

void
Export::write_type(const Type* type)
{
  // We don't want to assign a reference number to a forward
  // declaration to a type which was defined later.
  type = type->forwarded();

  Type_refs::const_iterator p = this->type_refs_.find(type);
  if (p != this->type_refs_.end())
    {
      // This type was already in the table.
      int index = p->second;
      go_assert(index != 0);
      char buf[30];
      snprintf(buf, sizeof buf, "<type %d>", index);
      this->write_c_string(buf);
      return;
    }

  const Named_type* named_type = type->named_type();
  const Forward_declaration_type* forward = type->forward_declaration_type();

  int index = this->type_index_;
  ++this->type_index_;

  char buf[30];
  snprintf(buf, sizeof buf, "<type %d ", index);
  this->write_c_string(buf);

  if (named_type != NULL || forward != NULL)
    {
      const Named_object* named_object;
      if (named_type != NULL)
	{
	  // The builtin types should have been predefined.
	  go_assert(!Linemap::is_predeclared_location(named_type->location())
		     || (named_type->named_object()->package()->package_name()
			 == "unsafe"));
	  named_object = named_type->named_object();
	}
      else
	named_object = forward->named_object();

      const Package* package = named_object->package();

      std::string s = "\"";
      if (package != NULL && !Gogo::is_hidden_name(named_object->name()))
	{
	  s += package->pkgpath();
	  s += '.';
	}
      s += named_object->name();
      s += "\" ";
      this->write_string(s);

      // It is possible that this type was imported indirectly, and is
      // not in a package in the import list.  If we have not
      // mentioned this package before, write out the package name
      // here so that any package importing this one will know it.
      if (package != NULL
	  && this->packages_.find(package) == this->packages_.end())
	{
	  this->write_c_string("\"");
	  this->write_string(package->package_name());
	  this->packages_.insert(package);
	  this->write_c_string("\" ");
	}

      // We must add a named type to the table now, since the
      // definition of the type may refer to the named type via a
      // pointer.
      this->type_refs_[type] = index;
    }

  type->export_type(this);

  this->write_c_string(">");

  if (named_type == NULL)
    this->type_refs_[type] = index;
}

// Export escape note.

void
Export::write_escape(std::string* note)
{
  if (note != NULL && *note != "esc:0x0")
    {
      this->write_c_string(" ");
      char buf[50];
      go_assert(note->find("esc:") != std::string::npos);
      snprintf(buf, sizeof buf, "<%s>", note->c_str());
      this->write_c_string(buf);
    }
}

// Add the builtin types to the export table.

void
Export::register_builtin_types(Gogo* gogo)
{
  this->register_builtin_type(gogo, "int8", BUILTIN_INT8);
  this->register_builtin_type(gogo, "int16", BUILTIN_INT16);
  this->register_builtin_type(gogo, "int32", BUILTIN_INT32);
  this->register_builtin_type(gogo, "int64", BUILTIN_INT64);
  this->register_builtin_type(gogo, "uint8", BUILTIN_UINT8);
  this->register_builtin_type(gogo, "uint16", BUILTIN_UINT16);
  this->register_builtin_type(gogo, "uint32", BUILTIN_UINT32);
  this->register_builtin_type(gogo, "uint64", BUILTIN_UINT64);
  this->register_builtin_type(gogo, "float32", BUILTIN_FLOAT32);
  this->register_builtin_type(gogo, "float64", BUILTIN_FLOAT64);
  this->register_builtin_type(gogo, "complex64", BUILTIN_COMPLEX64);
  this->register_builtin_type(gogo, "complex128", BUILTIN_COMPLEX128);
  this->register_builtin_type(gogo, "int", BUILTIN_INT);
  this->register_builtin_type(gogo, "uint", BUILTIN_UINT);
  this->register_builtin_type(gogo, "uintptr", BUILTIN_UINTPTR);
  this->register_builtin_type(gogo, "bool", BUILTIN_BOOL);
  this->register_builtin_type(gogo, "string", BUILTIN_STRING);
  this->register_builtin_type(gogo, "error", BUILTIN_ERROR);
  this->register_builtin_type(gogo, "byte", BUILTIN_BYTE);
  this->register_builtin_type(gogo, "rune", BUILTIN_RUNE);
}

// Register one builtin type in the export table.

void
Export::register_builtin_type(Gogo* gogo, const char* name, Builtin_code code)
{
  Named_object* named_object = gogo->lookup_global(name);
  go_assert(named_object != NULL && named_object->is_type());
  std::pair<Type_refs::iterator, bool> ins =
    this->type_refs_.insert(std::make_pair(named_object->type_value(), code));
  go_assert(ins.second);

  // We also insert the underlying type.  We can see the underlying
  // type at least for string and bool.  We skip the type aliases byte
  // and rune here.
  if (code != BUILTIN_BYTE && code != BUILTIN_RUNE)
    {
      Type* real_type = named_object->type_value()->real_type();
      ins = this->type_refs_.insert(std::make_pair(real_type, code));
      go_assert(ins.second);
    }
}

// Class Export::Stream.

Export::Stream::Stream()
{
  this->sha1_helper_ = go_create_sha1_helper();
  go_assert(this->sha1_helper_ != NULL);
}

Export::Stream::~Stream()
{
}

// Write bytes to the stream.  This keeps a checksum of bytes as they
// go by.

void
Export::Stream::write_and_sum_bytes(const char* bytes, size_t length)
{
  this->sha1_helper_->process_bytes(bytes, length);
  this->do_write(bytes, length);
}

// Get the checksum.

std::string
Export::Stream::checksum()
{
  std::string rval = this->sha1_helper_->finish();
  delete this->sha1_helper_;
  return rval;
}

// Write the checksum string to the export data.

void
Export::Stream::write_checksum(const std::string& s)
{
  this->do_write(s.data(), s.length());
}

// Class Stream_to_section.

Stream_to_section::Stream_to_section()
{
}

// Write data to a section.

void
Stream_to_section::do_write(const char* bytes, size_t length)
{
  go_write_export_data (bytes, length);
}
