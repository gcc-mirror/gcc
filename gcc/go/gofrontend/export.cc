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

#include "go-linemap.h"
#include "backend.h"

// This file handles exporting global declarations.

// Class Export.

const int Export::magic_len;

// Current version magic string.
const char Export::cur_magic[Export::magic_len] =
  {
    'v', '3', ';', '\n'
  };

// Magic strings for previous versions (still supported).
const char Export::v1_magic[Export::magic_len] =
  {
    'v', '1', ';', '\n'
  };
const char Export::v2_magic[Export::magic_len] =
  {
    'v', '2', ';', '\n'
  };

const int Export::checksum_len;

// Constructor.

Export::Export(Stream* stream)
  : stream_(stream), type_index_(1), packages_()
{
  go_assert(Export::checksum_len == Go_sha1_helper::checksum_len);
}

// Type hash table operations, treating aliases as distinct.

class Type_hash_alias_identical
{
 public:
  unsigned int
  operator()(const Type* type) const
  {
    return type->hash_for_method(NULL,
				 (Type::COMPARE_ERRORS
				  | Type::COMPARE_TAGS
				  | Type::COMPARE_EMBEDDED_INTERFACES
				  | Type::COMPARE_ALIASES));
  }
};

class Type_alias_identical
{
 public:
  bool
  operator()(const Type* t1, const Type* t2) const
  {
    return Type::are_identical(t1, t2,
			       (Type::COMPARE_ERRORS
				| Type::COMPARE_TAGS
                                | Type::COMPARE_EMBEDDED_INTERFACES
				| Type::COMPARE_ALIASES),
			       NULL);
  }
};

// Mapping from Type objects to a constant index.  This would be nicer
// as a field in Export, but then export.h would have to #include
// types.h.

typedef Unordered_map_hash(const Type*, int, Type_hash_alias_identical,
			   Type_alias_identical) Type_refs;

static Type_refs type_refs;

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

  // We don't export various special functions.
  if (Gogo::is_special_name(no->name()))
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

  // Assign indexes to all exported types and types referenced by
  // exported types, and collect all packages mentioned.
  Unordered_set(const Package*) type_imports;
  int unexported_type_index = this->prepare_types(&exports, &type_imports);

  // Although the export data is readable, at least this version is,
  // it is conceptually a binary format.  Start with a four byte
  // version number.
  this->write_bytes(Export::cur_magic, Export::magic_len);

  // The package name.
  this->write_c_string("package ");
  this->write_string(package_name);
  this->write_c_string("\n");

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
  this->write_c_string("\n");

  this->write_packages(packages);

  this->write_imports(imports, type_imports);

  this->write_imported_init_fns(package_name, import_init_fn,
				imported_init_fns);

  // FIXME: It might be clever to add something about the processor
  // and ABI being used, although ideally any problems in that area
  // would be caught by the linker.

  // Write out all the types, both exported and not.
  this->write_types(unexported_type_index);

  // Write out the non-type export data.
  for (std::vector<Named_object*>::const_iterator p = exports.begin();
       p != exports.end();
       ++p)
    {
      if (!(*p)->is_type())
	(*p)->export_named_object(this);
    }

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
  s += "\n";
  this->stream_->write_checksum(s);
}

// Traversal class to find referenced types.

class Find_types_to_prepare : public Traverse
{
 public:
  Find_types_to_prepare(Export* exp,
			Unordered_set(const Package*)* imports)
    : Traverse(traverse_types),
      exp_(exp), imports_(imports)
  { }

  int
  type(Type* type);

  // Traverse the components of a function type.
  void
  traverse_function(Function_type*);

  // Traverse the methods of a named type, and register its package.
  void
  traverse_named_type(Named_type*);

 private:
  // Exporters.
  Export* exp_;
  // List of packages we are building.
  Unordered_set(const Package*)* imports_;
};

// Set type index of referenced type, record package imports, and make
// sure we traverse methods of named types.

int
Find_types_to_prepare::type(Type* type)
{
  // Skip forwarders; don't try to give them a type index.
  if (type->forward_declaration_type() != NULL)
    return TRAVERSE_CONTINUE;

  // Skip the void type, which we'll see when exporting
  // unsafe.Pointer.  The void type is not itself exported, because
  // Pointer_type::do_export checks for it.
  if (type->is_void_type())
    return TRAVERSE_SKIP_COMPONENTS;

  // Skip abstract types.  We should never see these in real code,
  // only in things like const declarations.
  if (type->is_abstract())
    return TRAVERSE_SKIP_COMPONENTS;

  // For interfaces make sure that embedded methods are sorted, since the
  // comparison function we use for indexing types relies on it (this call has
  // to happen before the set_type_index call below).
  if (type->classification() == Type::TYPE_INTERFACE)
    {
      Interface_type* it = type->interface_type();
      if (it != NULL)
        it->sort_embedded();
    }

  if (!this->exp_->set_type_index(type))
    {
      // We've already seen this type.
      return TRAVERSE_SKIP_COMPONENTS;
    }

  // At this stage of compilation traversing interface types traverses
  // the final list of methods, but we export the locally defined
  // methods.  If there is an embedded interface type we need to make
  // sure to export that.  Check classification, rather than calling
  // the interface_type method, because we want to handle named types
  // below.
  if (type->classification() == Type::TYPE_INTERFACE)
    {
      Interface_type* it = type->interface_type();
      const Typed_identifier_list* methods = it->local_methods();
      if (methods != NULL)
	{
	  for (Typed_identifier_list::const_iterator p = methods->begin();
	       p != methods->end();
	       ++p)
	    {
	      if (p->name().empty())
		Type::traverse(p->type(), this);
	      else
		this->traverse_function(p->type()->function_type());
	    }
	}
      return TRAVERSE_SKIP_COMPONENTS;
    }

  Named_type* nt = type->named_type();
  if (nt != NULL)
    this->traverse_named_type(nt);

  return TRAVERSE_CONTINUE;
}

// Traverse the types in a function type.  We don't need the function
// type itself, just the receiver, parameter, and result types.

void
Find_types_to_prepare::traverse_function(Function_type* type)
{
  go_assert(type != NULL);
  if (this->remember_type(type))
    return;
  const Typed_identifier* receiver = type->receiver();
  if (receiver != NULL)
    Type::traverse(receiver->type(), this);
  const Typed_identifier_list* parameters = type->parameters();
  if (parameters != NULL)
    parameters->traverse(this);
  const Typed_identifier_list* results = type->results();
  if (results != NULL)
    results->traverse(this);
}

// Traverse the methods of a named type, and record its package.

void
Find_types_to_prepare::traverse_named_type(Named_type* nt)
{
  const Package* package = nt->named_object()->package();
  if (package != NULL)
    this->imports_->insert(package);

  // We have to traverse the methods of named types, because we are
  // going to export them.  This is not done by ordinary type
  // traversal.
  const Bindings* methods = nt->local_methods();
  if (methods != NULL)
    {
      for (Bindings::const_definitions_iterator pm =
	     methods->begin_definitions();
	   pm != methods->end_definitions();
	   ++pm)
	{
	  Function* fn = (*pm)->func_value();
	  this->traverse_function(fn->type());
	  if (fn->export_for_inlining())
	    fn->block()->traverse(this);
	}

      for (Bindings::const_declarations_iterator pm =
	     methods->begin_declarations();
	   pm != methods->end_declarations();
	   ++pm)
	{
	  Named_object* mno = pm->second;
	  if (mno->is_function_declaration())
	    this->traverse_function(mno->func_declaration_value()->type());
	}
    }
}

// Prepare to export types by assigning a type index to every exported
// type and every type referenced by an exported type.  Also collect
// all the packages we see in types, so that if we refer to any types
// from indirectly imported packages we can tell the importer about
// the package.  This returns the number of exported types.

int
Export::prepare_types(const std::vector<Named_object*>* exports,
		      Unordered_set(const Package*)* imports)
{
  // Assign indexes to all the exported types.
  for (std::vector<Named_object*>::const_iterator p = exports->begin();
       p != exports->end();
       ++p)
    {
      if (!(*p)->is_type())
	continue;
      Interface_type* it = (*p)->type_value()->interface_type();
      if (it != NULL)
        it->sort_embedded();
      this->set_type_index((*p)->type_value());
    }

  int ret = this->type_index_;

  // Use a single instance of the traversal class because traversal
  // classes keep track of which types they've already seen.  That
  // lets us avoid type reference loops.
  Find_types_to_prepare find(this, imports);

  // Traverse all the exported objects and assign indexes to all types.
  for (std::vector<Named_object*>::const_iterator p = exports->begin();
       p != exports->end();
       ++p)
    {
      Named_object* no = *p;
      switch (no->classification())
	{
	case Named_object::NAMED_OBJECT_CONST:
	  {
	    Type* t = no->const_value()->type();
	    if (t != NULL && !t->is_abstract())
	      Type::traverse(t, &find);
	  }
	  break;

	case Named_object::NAMED_OBJECT_TYPE:
	  Type::traverse(no->type_value()->real_type(), &find);
	  find.traverse_named_type(no->type_value());
	  break;

	case Named_object::NAMED_OBJECT_VAR:
	  Type::traverse(no->var_value()->type(), &find);
	  break;

	case Named_object::NAMED_OBJECT_FUNC:
	  {
	    Function* fn = no->func_value();
	    find.traverse_function(fn->type());
	    if (fn->export_for_inlining())
	      fn->block()->traverse(&find);
	  }
	  break;

	case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
	  find.traverse_function(no->func_declaration_value()->type());
	  break;

	default:
	  // We shouldn't see anything else.  If we do we'll give an
	  // error later when we try to actually export it.
	  break;
	}
    }

  return ret;
}

// Give a type an index if it doesn't already have one.  Return true
// if we set the type index, false if it was already known.

bool
Export::set_type_index(Type* type)
{
  type = type->forwarded();

  std::pair<Type_refs::iterator, bool> ins =
    type_refs.insert(std::make_pair(type, 0));
  if (!ins.second)
    {
      // We've already seen this type.
      return false;
    }

  int index = this->type_index_;
  ++this->type_index_;
  ins.first->second = index;

  return true;
}

// Sort packages.

static bool
packages_compare(const Package* a, const Package* b)
{
  if (a->package_name() < b->package_name())
    return true;
  else if (a->package_name() > b->package_name())
    return false;

  if (a->pkgpath() < b->pkgpath())
    return true;
  else if (a->pkgpath() > b->pkgpath())
    return false;

  // In principle if we get here then a == b.  Try to do something sensible
  // even if the import information is inconsistent.
  if (a->pkgpath_symbol() < b->pkgpath_symbol())
    return true;
  else if (a->pkgpath_symbol() > b->pkgpath_symbol())
    return false;

  return a < b;
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
      this->write_c_string("\n");
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
Export::write_imports(const std::map<std::string, Package*>& imports,
		      const Unordered_set(const Package*)& type_imports)
{
  // Sort the imports for more consistent output.
  Unordered_set(const Package*) seen;
  std::vector<std::pair<std::string, Package*> > sorted_imports;
  for (std::map<std::string, Package*>::const_iterator p = imports.begin();
       p != imports.end();
       ++p)
    {
      sorted_imports.push_back(std::make_pair(p->first, p->second));
      seen.insert(p->second);
    }

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
      this->write_c_string("\"\n");

      this->packages_.insert(p->second);
    }

  // Write out a separate list of indirectly imported packages.
  std::vector<const Package*> indirect_imports;
  for (Unordered_set(const Package*)::const_iterator p =
	 type_imports.begin();
       p != type_imports.end();
       ++p)
    {
      if (seen.find(*p) == seen.end())
	indirect_imports.push_back(*p);
    }

  std::sort(indirect_imports.begin(), indirect_imports.end(),
	    packages_compare);

  for (std::vector<const Package*>::const_iterator p =
	 indirect_imports.begin();
       p != indirect_imports.end();
       ++p)
    {
      this->write_c_string("indirectimport ");
      this->write_string((*p)->package_name());
      this->write_c_string(" ");
      this->write_string((*p)->pkgpath());
      this->write_c_string("\n");
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
      this->write_c_string("\n");
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

      if (ii->init_name() == import_init_fn)
	continue;

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
  this->write_c_string("\n");

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
  this->write_c_string("\n");
}

// Write the types to the export stream.

void
Export::write_types(int unexported_type_index)
{
  // Map from type index to type.
  std::vector<const Type*> types(static_cast<size_t>(this->type_index_));
  for (Type_refs::const_iterator p = type_refs.begin();
       p != type_refs.end();
       ++p)
    {
      if (p->second >= 0)
	types.at(p->second) = p->first;
    }

  // Write the type information to a buffer.
  Stream_to_string type_data;
  Export::Stream* orig_stream = this->stream_;
  this->stream_ = &type_data;

  std::vector<size_t> type_sizes(static_cast<size_t>(this->type_index_));
  type_sizes[0] = 0;

  // Start at 1 because type index 0 is not used.
  size_t start_size = 0;
  for (int i = 1; i < this->type_index_; ++i)
    {
      this->write_type_definition(types[i], i);

      size_t cur_size = type_data.string().size();
      type_sizes[i] = cur_size - start_size;
      start_size = cur_size;
    }

  // Back to original stream.
  this->stream_ = orig_stream;

  // The line "types MAXP1 EXPORTEDP1 SIZES..." appears before the
  // types.  MAXP1 is one more than the maximum type index used; that
  // is, it is the size of the array we need to allocate to hold all
  // the values.  Indexes 1 up to but not including EXPORTEDP1 are the
  // exported types.  The other types are not exported.  SIZES... is a
  // list of MAXP1-1 entries listing the size of the type definition
  // for each type, starting at index 1.
  char buf[100];
  snprintf(buf, sizeof buf, "types %d %d", this->type_index_,
	   unexported_type_index);
  this->write_c_string(buf);

  // Start at 1 because type index 0 is not used.
  for (int i = 1; i < this->type_index_; ++i)
    {
      snprintf(buf, sizeof buf, " %lu",
	       static_cast<unsigned long>(type_sizes[i]));
      this->write_c_string(buf);
    }
  this->write_c_string("\n");
  this->write_string(type_data.string());
}

// Write a single type to the export stream.

void
Export::write_type_definition(const Type* type, int index)
{
  this->write_c_string("type ");

  char buf[30];
  snprintf(buf, sizeof buf, "%d ", index);
  this->write_c_string(buf);

  const Named_type* nt = type->named_type();
  if (nt != NULL)
    {
      const Named_object* no = nt->named_object();
      const Package* package = no->package();

      this->write_c_string("\"");
      if (package != NULL && !Gogo::is_hidden_name(no->name()))
	{
	  this->write_string(package->pkgpath());
	  this->write_c_string(".");
	}
      this->write_string(nt->named_object()->name());
      this->write_c_string("\" ");

      if (nt->is_alias())
	this->write_c_string("= ");
    }

  type->export_type(this);

  // Type::export_type will print a newline for a named type, but not
  // otherwise.
  if (nt == NULL)
    this->write_c_string("\n");
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

// Return the index of a type.

int
Export::type_index(const Type* type)
{
  type = type->forwarded();
  Type_refs::const_iterator p = type_refs.find(type);
  go_assert(p != type_refs.end());
  int index = p->second;
  go_assert(index != 0);
  return index;
}

// Export a type.

void
Export::write_type(const Type* type)
{
  int index = this->type_index(type);
  char buf[30];
  snprintf(buf, sizeof buf, "<type %d>", index);
  this->write_c_string(buf);
}

// Export a type to a function body.

void
Export::write_type_to(const Type* type, Export_function_body* efb)
{
  int index = this->type_index(type);
  char buf[30];
  snprintf(buf, sizeof buf, "<type %d>", index);
  efb->write_c_string(buf);
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
    type_refs.insert(std::make_pair(named_object->type_value(), code));
  go_assert(ins.second);

  // We also insert the underlying type.  We can see the underlying
  // type at least for string and bool.  It's OK if this insert
  // fails--we expect duplications here, and it doesn't matter when
  // they occur.
  Type* real_type = named_object->type_value()->real_type();
  type_refs.insert(std::make_pair(real_type, code));
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

Stream_to_section::Stream_to_section(Backend* backend)
    : backend_(backend)
{
}

// Write data to a section.

void
Stream_to_section::do_write(const char* bytes, size_t length)
{
  this->backend_->write_export_data (bytes, length);
}
