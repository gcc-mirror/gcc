// export.cc -- Export declarations in Go frontend.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "sha1.h"

#include "go-c.h"

#include "gogo.h"
#include "types.h"
#include "statements.h"
#include "export.h"

// This file handles exporting global declarations.

// Class Export.

// Version 1 magic number.

const int Export::v1_magic_len;

const char Export::v1_magic[Export::v1_magic_len] =
  {
    'v', '1', ';', '\n'
  };

const int Export::v1_checksum_len;

// Constructor.

Export::Export(Stream* stream)
  : stream_(stream), type_refs_(), type_index_(1)
{
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
		       const std::string& unique_prefix,
		       int package_priority,
		       const std::map<std::string, Package*>& imports,
		       const std::string& import_init_fn,
		       const std::set<Import_init>& imported_init_fns,
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
  // verison number.
  this->write_bytes(Export::v1_magic, Export::v1_magic_len);

  // The package name.
  this->write_c_string("package ");
  this->write_string(package_name);
  this->write_c_string(";\n");

  // The unique prefix.  This prefix is used for all global symbols.
  this->write_c_string("prefix ");
  this->write_string(unique_prefix);
  this->write_c_string(";\n");

  // The package priority.
  char buf[100];
  snprintf(buf, sizeof buf, "priority %d;\n", package_priority);
  this->write_c_string(buf);

  this->write_imports(imports);

  this->write_imported_init_fns(package_name, package_priority, import_init_fn,
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
  std::vector<std::pair<std::string, Package*> > imp;
  for (std::map<std::string, Package*>::const_iterator p = imports.begin();
       p != imports.end();
       ++p)
    imp.push_back(std::make_pair(p->first, p->second));

  std::sort(imp.begin(), imp.end(), import_compare);

  for (std::vector<std::pair<std::string, Package*> >::const_iterator p =
	 imp.begin();
       p != imp.end();
       ++p)
    {
      this->write_c_string("import ");
      this->write_string(p->second->name());
      this->write_c_string(" ");
      this->write_string(p->second->unique_prefix());
      this->write_c_string(" \"");
      this->write_string(p->first);
      this->write_c_string("\";\n");
    }
}

// Write out the initialization functions which need to run for this
// package.

void
Export::write_imported_init_fns(
    const std::string& package_name,
    int priority,
    const std::string& import_init_fn,
    const std::set<Import_init>& imported_init_fns)
{
  if (import_init_fn.empty() && imported_init_fns.empty())
    return;

  this->write_c_string("init");

  if (!import_init_fn.empty())
    {
      this->write_c_string(" ");
      this->write_string(package_name);
      this->write_c_string(" ");
      this->write_string(import_init_fn);
      char buf[100];
      snprintf(buf, sizeof buf, " %d", priority);
      this->write_c_string(buf);
    }

  if (!imported_init_fns.empty())
    {
      // Sort the list of functions for more consistent output.
      std::vector<Import_init> v;
      for (std::set<Import_init>::const_iterator p = imported_init_fns.begin();
	   p != imported_init_fns.end();
	   ++p)
	v.push_back(*p);
      std::sort(v.begin(), v.end());

      for (std::vector<Import_init>::const_iterator p = v.begin();
	   p != v.end();
	   ++p)
	{
	  this->write_c_string(" ");
	  this->write_string(p->package_name());
	  this->write_c_string(" ");
	  this->write_string(p->init_name());
	  char buf[100];
	  snprintf(buf, sizeof buf, " %d", p->priority());
	  this->write_c_string(buf);
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
		     || (named_type->named_object()->package()->name()
			 == "unsafe"));
	  named_object = named_type->named_object();
	}
      else
	named_object = forward->named_object();

      const Package* package = named_object->package();

      std::string s = "\"";
      if (package != NULL && !Gogo::is_hidden_name(named_object->name()))
	{
	  s += package->unique_prefix();
	  s += '.';
	  s += package->name();
	  s += '.';
	}
      s += named_object->name();
      s += "\" ";
      this->write_string(s);

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
  this->checksum_ = new sha1_ctx;
  memset(this->checksum_, 0, sizeof(sha1_ctx));
  sha1_init_ctx(this->checksum_);
}

Export::Stream::~Stream()
{
}

// Write bytes to the stream.  This keeps a checksum of bytes as they
// go by.

void
Export::Stream::write_and_sum_bytes(const char* bytes, size_t length)
{
  sha1_process_bytes(bytes, length, this->checksum_);
  this->do_write(bytes, length);
}

// Get the checksum.

std::string
Export::Stream::checksum()
{
  // Use a union to provide the required alignment.
  union
  {
    char checksum[Export::v1_checksum_len];
    long align;
  } u;
  sha1_finish_ctx(this->checksum_, u.checksum);
  return std::string(u.checksum, Export::v1_checksum_len);
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
