// gogo.cc -- Go frontend parsed representation.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include <fstream>

#include "filenames.h"

#include "go-c.h"
#include "go-diagnostics.h"
#include "go-encode-id.h"
#include "go-dump.h"
#include "go-optimize.h"
#include "lex.h"
#include "types.h"
#include "statements.h"
#include "expressions.h"
#include "runtime.h"
#include "import.h"
#include "export.h"
#include "backend.h"
#include "gogo.h"

// Class Gogo.

Gogo::Gogo(Backend* backend, Linemap* linemap, int, int pointer_size)
  : backend_(backend),
    linemap_(linemap),
    package_(NULL),
    functions_(),
    globals_(new Bindings(NULL)),
    file_block_names_(),
    imports_(),
    imported_unsafe_(false),
    current_file_imported_unsafe_(false),
    current_file_imported_embed_(false),
    packages_(),
    init_functions_(),
    var_deps_(),
    need_init_fn_(false),
    init_fn_name_(),
    imported_init_fns_(),
    pkgpath_(),
    pkgpath_symbol_(),
    prefix_(),
    pkgpath_set_(false),
    pkgpath_from_option_(false),
    prefix_from_option_(false),
    relative_import_path_(),
    c_header_(),
    check_divide_by_zero_(true),
    check_divide_overflow_(true),
    compiling_runtime_(false),
    debug_escape_level_(0),
    debug_optimization_(false),
    nil_check_size_threshold_(4096),
    need_eqtype_(false),
    verify_types_(),
    interface_types_(),
    specific_type_functions_(),
    specific_type_functions_are_written_(false),
    named_types_are_converted_(false),
    analysis_sets_(),
    gc_roots_(),
    type_descriptors_(),
    imported_inlinable_functions_(),
    imported_inline_functions_()
{
  const Location loc = Linemap::predeclared_location();

  Named_type* uint8_type = Type::make_integer_type("uint8", true, 8,
						   RUNTIME_TYPE_KIND_UINT8);
  this->add_named_type(uint8_type);
  this->add_named_type(Type::make_integer_type("uint16", true,  16,
					       RUNTIME_TYPE_KIND_UINT16));
  this->add_named_type(Type::make_integer_type("uint32", true,  32,
					       RUNTIME_TYPE_KIND_UINT32));
  this->add_named_type(Type::make_integer_type("uint64", true,  64,
					       RUNTIME_TYPE_KIND_UINT64));

  this->add_named_type(Type::make_integer_type("int8",  false,   8,
					       RUNTIME_TYPE_KIND_INT8));
  this->add_named_type(Type::make_integer_type("int16", false,  16,
					       RUNTIME_TYPE_KIND_INT16));
  Named_type* int32_type = Type::make_integer_type("int32", false,  32,
						   RUNTIME_TYPE_KIND_INT32);
  this->add_named_type(int32_type);
  this->add_named_type(Type::make_integer_type("int64", false,  64,
					       RUNTIME_TYPE_KIND_INT64));

  this->add_named_type(Type::make_float_type("float32", 32,
					     RUNTIME_TYPE_KIND_FLOAT32));
  this->add_named_type(Type::make_float_type("float64", 64,
					     RUNTIME_TYPE_KIND_FLOAT64));

  this->add_named_type(Type::make_complex_type("complex64", 64,
					       RUNTIME_TYPE_KIND_COMPLEX64));
  this->add_named_type(Type::make_complex_type("complex128", 128,
					       RUNTIME_TYPE_KIND_COMPLEX128));

  int int_type_size = pointer_size;
  if (int_type_size < 32)
    int_type_size = 32;
  this->add_named_type(Type::make_integer_type("uint", true,
					       int_type_size,
					       RUNTIME_TYPE_KIND_UINT));
  Named_type* int_type = Type::make_integer_type("int", false, int_type_size,
						 RUNTIME_TYPE_KIND_INT);
  this->add_named_type(int_type);

  this->add_named_type(Type::make_integer_type("uintptr", true,
					       pointer_size,
					       RUNTIME_TYPE_KIND_UINTPTR));

  // "byte" is an alias for "uint8".
  uint8_type->integer_type()->set_is_byte();
  this->add_named_type(Type::make_integer_type_alias("byte", uint8_type));

  // "rune" is an alias for "int32".
  int32_type->integer_type()->set_is_rune();
  this->add_named_type(Type::make_integer_type_alias("rune", int32_type));

  this->add_named_type(Type::make_named_bool_type());

  this->add_named_type(Type::make_named_string_type());

  // "error" is interface { Error() string }.
  {
    Typed_identifier_list *methods = new Typed_identifier_list;
    Typed_identifier_list *results = new Typed_identifier_list;
    results->push_back(Typed_identifier("", Type::lookup_string_type(), loc));
    Type *method_type = Type::make_function_type(NULL, NULL, results, loc);
    methods->push_back(Typed_identifier("Error", method_type, loc));
    Interface_type *error_iface = Type::make_interface_type(methods, loc);
    error_iface->finalize_methods();
    Named_type *error_type = Named_object::make_type("error", NULL, error_iface, loc)->type_value();
    this->add_named_type(error_type);
  }

  // "any" is an alias for the empty interface type.
  {
    Type* empty = Type::make_empty_interface_type(loc);
    Named_object* no = Named_object::make_type("any", NULL, empty, loc);
    Named_type* nt = no->type_value();
    nt->set_is_alias();
    this->add_named_type(nt);
  }

  this->globals_->add_constant(Typed_identifier("true",
						Type::make_boolean_type(),
						loc),
			       NULL,
			       Expression::make_boolean(true, loc),
			       0);
  this->globals_->add_constant(Typed_identifier("false",
						Type::make_boolean_type(),
						loc),
			       NULL,
			       Expression::make_boolean(false, loc),
			       0);

  this->globals_->add_constant(Typed_identifier("nil", Type::make_nil_type(),
						loc),
			       NULL,
			       Expression::make_nil(loc),
			       0);

  Type* abstract_int_type = Type::make_abstract_integer_type();
  this->globals_->add_constant(Typed_identifier("iota", abstract_int_type,
						loc),
			       NULL,
			       Expression::make_iota(),
			       0);

  Function_type* new_type = Type::make_function_type(NULL, NULL, NULL, loc);
  new_type->set_is_varargs();
  new_type->set_is_builtin();
  this->globals_->add_function_declaration("new", NULL, new_type, loc);

  Function_type* make_type = Type::make_function_type(NULL, NULL, NULL, loc);
  make_type->set_is_varargs();
  make_type->set_is_builtin();
  this->globals_->add_function_declaration("make", NULL, make_type, loc);

  Typed_identifier_list* len_result = new Typed_identifier_list();
  len_result->push_back(Typed_identifier("", int_type, loc));
  Function_type* len_type = Type::make_function_type(NULL, NULL, len_result,
						     loc);
  len_type->set_is_builtin();
  this->globals_->add_function_declaration("len", NULL, len_type, loc);

  Typed_identifier_list* cap_result = new Typed_identifier_list();
  cap_result->push_back(Typed_identifier("", int_type, loc));
  Function_type* cap_type = Type::make_function_type(NULL, NULL, len_result,
						     loc);
  cap_type->set_is_builtin();
  this->globals_->add_function_declaration("cap", NULL, cap_type, loc);

  Function_type* print_type = Type::make_function_type(NULL, NULL, NULL, loc);
  print_type->set_is_varargs();
  print_type->set_is_builtin();
  this->globals_->add_function_declaration("print", NULL, print_type, loc);

  print_type = Type::make_function_type(NULL, NULL, NULL, loc);
  print_type->set_is_varargs();
  print_type->set_is_builtin();
  this->globals_->add_function_declaration("println", NULL, print_type, loc);

  Type *empty = Type::make_empty_interface_type(loc);
  Typed_identifier_list* panic_parms = new Typed_identifier_list();
  panic_parms->push_back(Typed_identifier("e", empty, loc));
  Function_type *panic_type = Type::make_function_type(NULL, panic_parms,
						       NULL, loc);
  panic_type->set_is_builtin();
  this->globals_->add_function_declaration("panic", NULL, panic_type, loc);

  Typed_identifier_list* recover_result = new Typed_identifier_list();
  recover_result->push_back(Typed_identifier("", empty, loc));
  Function_type* recover_type = Type::make_function_type(NULL, NULL,
							 recover_result,
							 loc);
  recover_type->set_is_builtin();
  this->globals_->add_function_declaration("recover", NULL, recover_type, loc);

  Function_type* close_type = Type::make_function_type(NULL, NULL, NULL, loc);
  close_type->set_is_varargs();
  close_type->set_is_builtin();
  this->globals_->add_function_declaration("close", NULL, close_type, loc);

  Typed_identifier_list* copy_result = new Typed_identifier_list();
  copy_result->push_back(Typed_identifier("", int_type, loc));
  Function_type* copy_type = Type::make_function_type(NULL, NULL,
						      copy_result, loc);
  copy_type->set_is_varargs();
  copy_type->set_is_builtin();
  this->globals_->add_function_declaration("copy", NULL, copy_type, loc);

  Function_type* append_type = Type::make_function_type(NULL, NULL, NULL, loc);
  append_type->set_is_varargs();
  append_type->set_is_builtin();
  this->globals_->add_function_declaration("append", NULL, append_type, loc);

  Function_type* complex_type = Type::make_function_type(NULL, NULL, NULL, loc);
  complex_type->set_is_varargs();
  complex_type->set_is_builtin();
  this->globals_->add_function_declaration("complex", NULL, complex_type, loc);

  Function_type* real_type = Type::make_function_type(NULL, NULL, NULL, loc);
  real_type->set_is_varargs();
  real_type->set_is_builtin();
  this->globals_->add_function_declaration("real", NULL, real_type, loc);

  Function_type* imag_type = Type::make_function_type(NULL, NULL, NULL, loc);
  imag_type->set_is_varargs();
  imag_type->set_is_builtin();
  this->globals_->add_function_declaration("imag", NULL, imag_type, loc);

  Function_type* delete_type = Type::make_function_type(NULL, NULL, NULL, loc);
  delete_type->set_is_varargs();
  delete_type->set_is_builtin();
  this->globals_->add_function_declaration("delete", NULL, delete_type, loc);
}

std::string
Gogo::pkgpath_for_symbol(const std::string& pkgpath)
{
  go_assert(!pkgpath.empty());
  return go_encode_id(pkgpath);
}

// Return a hash code for a string, given a starting hash.

unsigned int
Gogo::hash_string(const std::string& s, unsigned int h)
{
  const char* p = s.data();
  size_t len = s.length();
  for (; len > 0; --len)
    {
      h ^= *p++;
      h*= 16777619;
    }
  return h;
}

// Get the package path to use for type reflection data.  This should
// ideally be unique across the entire link.

const std::string&
Gogo::pkgpath() const
{
  go_assert(this->pkgpath_set_);
  return this->pkgpath_;
}

// Set the package path from the -fgo-pkgpath command line option.

void
Gogo::set_pkgpath(const std::string& arg)
{
  go_assert(!this->pkgpath_set_);
  this->pkgpath_ = arg;
  this->pkgpath_set_ = true;
  this->pkgpath_from_option_ = true;
}

// Get the package path to use for symbol names.

const std::string&
Gogo::pkgpath_symbol() const
{
  go_assert(this->pkgpath_set_);
  return this->pkgpath_symbol_;
}

// Set the unique prefix to use to determine the package path, from
// the -fgo-prefix command line option.

void
Gogo::set_prefix(const std::string& arg)
{
  go_assert(!this->prefix_from_option_);
  this->prefix_ = arg;
  this->prefix_from_option_ = true;
}

// Munge name for use in an error message.

std::string
Gogo::message_name(const std::string& name)
{
  return go_localize_identifier(Gogo::unpack_hidden_name(name).c_str());
}

// Get the package name.

const std::string&
Gogo::package_name() const
{
  go_assert(this->package_ != NULL);
  return this->package_->package_name();
}

// Set the package name.

void
Gogo::set_package_name(const std::string& package_name,
		       Location location)
{
  if (this->package_ != NULL)
    {
      if (this->package_->package_name() != package_name)
	go_error_at(location, "expected package %<%s%>",
		    Gogo::message_name(this->package_->package_name()).c_str());
      return;
    }

  // Now that we know the name of the package we are compiling, set
  // the package path to use for reflect.Type.PkgPath and global
  // symbol names.
  if (this->pkgpath_set_)
    this->pkgpath_symbol_ = Gogo::pkgpath_for_symbol(this->pkgpath_);
  else
    {
      if (!this->prefix_from_option_ && package_name == "main")
	{
	  this->pkgpath_ = package_name;
	  this->pkgpath_symbol_ = Gogo::pkgpath_for_symbol(package_name);
	}
      else
	{
	  if (!this->prefix_from_option_)
	    this->prefix_ = "go";
	  this->pkgpath_ = this->prefix_ + '.' + package_name;
	  this->pkgpath_symbol_ = (Gogo::pkgpath_for_symbol(this->prefix_) + '.'
				   + Gogo::pkgpath_for_symbol(package_name));
	}
      this->pkgpath_set_ = true;
    }

  this->package_ = this->register_package(this->pkgpath_,
					  this->pkgpath_symbol_, location);
  this->package_->set_package_name(package_name, location);

  if (this->is_main_package())
    {
      // Declare "main" as a function which takes no parameters and
      // returns no value.
      Location uloc = Linemap::unknown_location();
      this->declare_function(Gogo::pack_hidden_name("main", false),
			     Type::make_function_type (NULL, NULL, NULL, uloc),
			     uloc);
    }
}

// Return whether this is the "main" package.  This is not true if
// -fgo-pkgpath or -fgo-prefix was used.

bool
Gogo::is_main_package() const
{
  return (this->package_name() == "main"
	  && !this->pkgpath_from_option_
	  && !this->prefix_from_option_);
}

// Import a package.

void
Gogo::import_package(const std::string& filename,
		     const std::string& local_name,
		     bool is_local_name_exported,
		     bool must_exist,
		     Location location)
{
  if (filename.empty())
    {
      go_error_at(location, "import path is empty");
      return;
    }

  const char *pf = filename.data();
  const char *pend = pf + filename.length();
  while (pf < pend)
    {
      unsigned int c;
      int adv = Lex::fetch_char(pf, &c);
      if (adv == 0)
	{
	  go_error_at(location, "import path contains invalid UTF-8 sequence");
	  return;
	}
      if (c == '\0')
	{
	  go_error_at(location, "import path contains NUL");
	  return;
	}
      if (c < 0x20 || c == 0x7f)
	{
	  go_error_at(location, "import path contains control character");
	  return;
	}
      if (c == '\\')
	{
	  go_error_at(location, "import path contains backslash; use slash");
	  return;
	}
      if (Lex::is_unicode_space(c))
	{
	  go_error_at(location, "import path contains space character");
	  return;
	}
      if (c < 0x7f && strchr("!\"#$%&'()*,:;<=>?[]^`{|}", c) != NULL)
	{
	  go_error_at(location,
                      "import path contains invalid character '%c'", c);
	  return;
	}
      pf += adv;
    }

  if (IS_ABSOLUTE_PATH(filename.c_str()))
    {
      go_error_at(location, "import path cannot be absolute path");
      return;
    }

  if (local_name == "init")
    go_error_at(location, "cannot import package as init");

  if (filename == "unsafe")
    {
      this->import_unsafe(local_name, is_local_name_exported, location);
      this->current_file_imported_unsafe_ = true;
      return;
    }

  if (filename == "embed")
    this->current_file_imported_embed_ = true;

  Imports::const_iterator p = this->imports_.find(filename);
  if (p != this->imports_.end())
    {
      Package* package = p->second;
      package->set_location(location);
      std::string ln = local_name;
      bool is_ln_exported = is_local_name_exported;
      if (ln.empty())
	{
	  ln = package->package_name();
	  go_assert(!ln.empty());
	  is_ln_exported = Lex::is_exported_name(ln);
	}
      if (ln == "_")
        ;
      else if (ln == ".")
	{
	  Bindings* bindings = package->bindings();
	  for (Bindings::const_declarations_iterator pd =
		 bindings->begin_declarations();
	       pd != bindings->end_declarations();
	       ++pd)
	    this->add_dot_import_object(pd->second);
          std::string dot_alias = "." + package->package_name();
          package->add_alias(dot_alias, location);
	}
      else
	{
          package->add_alias(ln, location);
	  ln = this->pack_hidden_name(ln, is_ln_exported);
	  this->package_->bindings()->add_package(ln, package);
	}
      return;
    }

  Import::Stream* stream = Import::open_package(filename, location,
						this->relative_import_path_);
  if (stream == NULL)
    {
      if (must_exist)
	go_error_at(location, "import file %qs not found", filename.c_str());
      return;
    }

  Import* imp = new Import(stream, location);
  imp->register_builtin_types(this);
  Package* package = imp->import(this, local_name, is_local_name_exported);
  if (package != NULL)
    {
      if (package->pkgpath() == this->pkgpath())
	go_error_at(location,
		    ("imported package uses same package path as package "
		     "being compiled (see %<-fgo-pkgpath%> option)"));

      this->imports_.insert(std::make_pair(filename, package));
    }

  imp->clear_stream();
  delete stream;

  // FIXME: we never delete imp; we may need it for inlinable functions.
}

Import_init *
Gogo::lookup_init(const std::string& init_name)
{
  Import_init tmp("", init_name, -1);
  Import_init_set::iterator it = this->imported_init_fns_.find(&tmp);
  return (it != this->imported_init_fns_.end()) ? *it : NULL;
}

// Add an import control function for an imported package to the list.

void
Gogo::add_import_init_fn(const std::string& package_name,
			 const std::string& init_name, int prio)
{
  for (Import_init_set::iterator p =
	 this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    {
      Import_init *ii = (*p);
      if (ii->init_name() == init_name)
	{
	  // If a test of package P1, built as part of package P1,
	  // imports package P2, and P2 imports P1 (perhaps
	  // indirectly), then we will see the same import name with
	  // different import priorities.  That is OK, so don't give
	  // an error about it.
	  if (ii->package_name() != package_name)
	    {
	      go_error_at(Linemap::unknown_location(),
		       "duplicate package initialization name %qs",
		       Gogo::message_name(init_name).c_str());
	      go_inform(Linemap::unknown_location(), "used by package %qs",
			Gogo::message_name(ii->package_name()).c_str());
	      go_inform(Linemap::unknown_location(), " and by package %qs",
			Gogo::message_name(package_name).c_str());
	    }
          ii->set_priority(prio);
          return;
	}
    }

  Import_init* nii = new Import_init(package_name, init_name, prio);
  this->imported_init_fns_.insert(nii);
}

// Return whether we are at the global binding level.

bool
Gogo::in_global_scope() const
{
  return this->functions_.empty();
}

// Return the current binding contour.

Bindings*
Gogo::current_bindings()
{
  if (!this->functions_.empty())
    return this->functions_.back().blocks.back()->bindings();
  else if (this->package_ != NULL)
    return this->package_->bindings();
  else
    return this->globals_;
}

const Bindings*
Gogo::current_bindings() const
{
  if (!this->functions_.empty())
    return this->functions_.back().blocks.back()->bindings();
  else if (this->package_ != NULL)
    return this->package_->bindings();
  else
    return this->globals_;
}

void
Gogo::update_init_priority(Import_init* ii,
                           std::set<const Import_init *>* visited)
{
  visited->insert(ii);
  int succ_prior = -1;

  for (std::set<std::string>::const_iterator pci =
           ii->precursors().begin();
       pci != ii->precursors().end();
       ++pci)
    {
      Import_init* succ = this->lookup_init(*pci);
      if (visited->find(succ) == visited->end())
        update_init_priority(succ, visited);
      succ_prior = std::max(succ_prior, succ->priority());
    }
  if (ii->priority() <= succ_prior)
    ii->set_priority(succ_prior + 1);
}

void
Gogo::recompute_init_priorities()
{
  std::set<Import_init *> nonroots;

  for (Import_init_set::const_iterator p =
           this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    {
      const Import_init *ii = *p;
      for (std::set<std::string>::const_iterator pci =
               ii->precursors().begin();
           pci != ii->precursors().end();
           ++pci)
        {
          Import_init* ii_init = this->lookup_init(*pci);
          nonroots.insert(ii_init);
        }
    }

  // Recursively update priorities starting at roots.
  std::set<const Import_init*> visited;
  for (Import_init_set::iterator p =
           this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    {
      Import_init* ii = *p;
      if (nonroots.find(ii) != nonroots.end())
        continue;
      update_init_priority(ii, &visited);
    }
}

// Add statements to INIT_STMTS which run the initialization
// functions for imported packages.  This is only used for the "main"
// package.

void
Gogo::init_imports(std::vector<Bstatement*>& init_stmts, Bfunction *bfunction)
{
  go_assert(this->is_main_package());

  if (this->imported_init_fns_.empty())
    return;

  Location unknown_loc = Linemap::unknown_location();
  Function_type* func_type =
      Type::make_function_type(NULL, NULL, NULL, unknown_loc);
  Btype* fntype = func_type->get_backend_fntype(this);

  // Recompute init priorities based on a walk of the init graph.
  recompute_init_priorities();

  // We must call them in increasing priority order.
  std::vector<const Import_init*> v;
  for (Import_init_set::const_iterator p =
	 this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    {
      // Don't include dummy inits. They are not real functions.
      if ((*p)->is_dummy())
        continue;
      if ((*p)->priority() < 0)
	go_error_at(Linemap::unknown_location(),
		    "internal error: failed to set init priority for %s",
		    (*p)->package_name().c_str());
      v.push_back(*p);
    }
  std::sort(v.begin(), v.end(), priority_compare);

  // We build calls to the init functions, which take no arguments.
  std::vector<Bexpression*> empty_args;
  for (std::vector<const Import_init*>::const_iterator p = v.begin();
       p != v.end();
       ++p)
    {
      const Import_init* ii = *p;
      std::string user_name = ii->package_name() + ".init";
      const std::string& init_name(ii->init_name());
      const unsigned int flags =
	(Backend::function_is_visible
	 | Backend::function_is_declaration
	 | Backend::function_is_inlinable);
      Bfunction* pfunc = this->backend()->function(fntype, user_name, init_name,
						   flags, unknown_loc);
      Bexpression* pfunc_code =
          this->backend()->function_code_expression(pfunc, unknown_loc);
      Bexpression* pfunc_call =
          this->backend()->call_expression(bfunction, pfunc_code, empty_args,
                                           NULL, unknown_loc);
      init_stmts.push_back(this->backend()->expression_statement(bfunction,
                                                                 pfunc_call));
    }
}

// Register global variables with the garbage collector.  We need to
// register all variables which can hold a pointer value.  They become
// roots during the mark phase.  We build a struct that is easy to
// hook into a list of roots.

// type gcRoot struct {
// 	decl    unsafe.Pointer // Pointer to variable.
//	size    uintptr        // Total size of variable.
// 	ptrdata uintptr        // Length of variable's gcdata.
// 	gcdata  *byte          // Pointer mask.
// }
//
// type gcRootList struct {
// 	next  *gcRootList
// 	count int
// 	roots [...]gcRoot
// }

// The last entry in the roots array has a NULL decl field.

void
Gogo::register_gc_vars(const std::vector<Named_object*>& var_gc,
		       std::vector<Bstatement*>& init_stmts,
                       Bfunction* init_bfn)
{
  if (var_gc.empty() && this->gc_roots_.empty())
    return;

  Type* pvt = Type::make_pointer_type(Type::make_void_type());
  Type* uintptr_type = Type::lookup_integer_type("uintptr");
  Type* byte_type = Type::lookup_integer_type("byte");
  Type* pointer_byte_type = Type::make_pointer_type(byte_type);
  Struct_type* root_type =
    Type::make_builtin_struct_type(4,
				   "decl", pvt,
				   "size", uintptr_type,
				   "ptrdata", uintptr_type,
				   "gcdata", pointer_byte_type);

  Location builtin_loc = Linemap::predeclared_location();
  unsigned long roots_len = var_gc.size() + this->gc_roots_.size();
  Expression* length = Expression::make_integer_ul(roots_len, NULL,
                                                   builtin_loc);
  Array_type* root_array_type = Type::make_array_type(root_type, length);
  root_array_type->set_is_array_incomparable();

  Type* int_type = Type::lookup_integer_type("int");
  Struct_type* root_list_type =
      Type::make_builtin_struct_type(3,
                                     "next", pvt,
				     "count", int_type,
                                     "roots", root_array_type);

  // Build an initializer for the roots array.

  Expression_list* roots_init = new Expression_list();

  for (std::vector<Named_object*>::const_iterator p = var_gc.begin();
       p != var_gc.end();
       ++p)
    {
      Expression_list* init = new Expression_list();

      Location no_loc = (*p)->location();
      Expression* decl = Expression::make_var_reference(*p, no_loc);
      Expression* decl_addr =
          Expression::make_unary(OPERATOR_AND, decl, no_loc);
      decl_addr->unary_expression()->set_does_not_escape();
      decl_addr = Expression::make_cast(pvt, decl_addr, no_loc);
      init->push_back(decl_addr);

      Expression* size =
	Expression::make_type_info(decl->type(),
				   Expression::TYPE_INFO_SIZE);
      init->push_back(size);

      Expression* ptrdata =
	Expression::make_type_info(decl->type(),
				   Expression::TYPE_INFO_BACKEND_PTRDATA);
      init->push_back(ptrdata);

      Expression* gcdata = Expression::make_ptrmask_symbol(decl->type());
      init->push_back(gcdata);

      Expression* root_ctor =
          Expression::make_struct_composite_literal(root_type, init, no_loc);
      roots_init->push_back(root_ctor);
    }

  for (std::vector<Expression*>::const_iterator p = this->gc_roots_.begin();
       p != this->gc_roots_.end();
       ++p)
    {
      Expression_list *init = new Expression_list();

      Expression* expr = *p;
      Location eloc = expr->location();
      init->push_back(Expression::make_cast(pvt, expr, eloc));

      Type* type = expr->type()->points_to();
      go_assert(type != NULL);

      Expression* size =
	Expression::make_type_info(type,
				   Expression::TYPE_INFO_SIZE);
      init->push_back(size);

      Expression* ptrdata =
	Expression::make_type_info(type,
				   Expression::TYPE_INFO_BACKEND_PTRDATA);
      init->push_back(ptrdata);

      Expression* gcdata = Expression::make_ptrmask_symbol(type);
      init->push_back(gcdata);

      Expression* root_ctor =
	Expression::make_struct_composite_literal(root_type, init, eloc);
      roots_init->push_back(root_ctor);
    }

  // Build a constructor for the struct.

  Expression_list* root_list_init = new Expression_list();
  root_list_init->push_back(Expression::make_nil(builtin_loc));
  root_list_init->push_back(Expression::make_integer_ul(roots_len, int_type,
							builtin_loc));

  Expression* roots_ctor =
      Expression::make_array_composite_literal(root_array_type, roots_init,
                                               builtin_loc);
  root_list_init->push_back(roots_ctor);

  Expression* root_list_ctor =
      Expression::make_struct_composite_literal(root_list_type, root_list_init,
                                                builtin_loc);

  Expression* root_addr = Expression::make_unary(OPERATOR_AND, root_list_ctor,
                                                 builtin_loc);
  root_addr->unary_expression()->set_is_gc_root();
  Expression* register_roots = Runtime::make_call(Runtime::REGISTER_GC_ROOTS,
                                                  builtin_loc, 1, root_addr);

  Translate_context context(this, NULL, NULL, NULL);
  Bexpression* bcall = register_roots->get_backend(&context);
  init_stmts.push_back(this->backend()->expression_statement(init_bfn, bcall));
}

// Build the list of type descriptors defined in this package. This is to help
// the reflect package to find compiler-generated types.

// type typeDescriptorList struct {
// 	 count int
// 	 types [...]unsafe.Pointer
// }

static Struct_type*
type_descriptor_list_type(unsigned long len)
{
  Location builtin_loc = Linemap::predeclared_location();
  Type* int_type = Type::lookup_integer_type("int");
  Type* ptr_type = Type::make_pointer_type(Type::make_void_type());
  // Avoid creating zero-length type.
  unsigned long nelems = (len != 0 ? len : 1);
  Expression* len_expr = Expression::make_integer_ul(nelems, NULL,
                                                     builtin_loc);
  Array_type* array_type = Type::make_array_type(ptr_type, len_expr);
  array_type->set_is_array_incomparable();
  Struct_type* list_type =
    Type::make_builtin_struct_type(2, "count", int_type,
                                   "types", array_type);
  return list_type;
}

void
Gogo::build_type_descriptor_list()
{
  // Create the list type
  Location builtin_loc = Linemap::predeclared_location();
  unsigned long len = this->type_descriptors_.size();
  Struct_type* list_type = type_descriptor_list_type(len);
  Btype* bt = list_type->get_backend(this);
  Btype* bat = list_type->field(1)->type()->get_backend(this);

  // Create the variable
  std::string name = this->type_descriptor_list_symbol(this->pkgpath_symbol());
  unsigned int flags = Backend::variable_is_constant;
  Bvariable* bv = this->backend()->implicit_variable(name, name, bt, flags, 0);

  // Build the initializer
  std::vector<unsigned long> indexes;
  std::vector<Bexpression*> vals;
  std::vector<Type*>::iterator p = this->type_descriptors_.begin();
  for (unsigned long i = 0; i < len; ++i, ++p)
    {
      Bexpression* bexpr = (*p)->type_descriptor_pointer(this,
                                                         builtin_loc);
      indexes.push_back(i);
      vals.push_back(bexpr);
    }
  Bexpression* barray =
    this->backend()->array_constructor_expression(bat, indexes, vals,
                                                  builtin_loc);

  Translate_context context(this, NULL, NULL, NULL);
  std::vector<Bexpression*> fields;
  Expression* len_expr = Expression::make_integer_ul(len, NULL,
                                                     builtin_loc);
  fields.push_back(len_expr->get_backend(&context));
  fields.push_back(barray);
  Bexpression* binit =
    this->backend()->constructor_expression(bt, fields, builtin_loc);

  this->backend()->implicit_variable_set_init(bv, name, bt, flags, binit);
}

// Register the type descriptors with the runtime.  This is to help
// the reflect package to find compiler-generated types.

void
Gogo::register_type_descriptors(std::vector<Bstatement*>& init_stmts,
                                Bfunction* init_bfn)
{
  // Create the list type
  Location builtin_loc = Linemap::predeclared_location();
  Struct_type* list_type = type_descriptor_list_type(1);
  Btype* bt = list_type->get_backend(this);

  // Collect type lists from transitive imports.
  std::vector<std::string> list_names;
  for (Import_init_set::iterator it = this->imported_init_fns_.begin();
       it != this->imported_init_fns_.end();
       ++it)
    {
      std::string pkgpath_symbol =
        this->pkgpath_symbol_from_init_fn_name((*it)->init_name());
      list_names.push_back(this->type_descriptor_list_symbol(pkgpath_symbol));
    }
  // Add the main package itself.
  list_names.push_back(this->type_descriptor_list_symbol("main"));

  // Build a list of lists.
  std::vector<unsigned long> indexes;
  std::vector<Bexpression*> vals;
  unsigned long i = 0;
  for (std::vector<std::string>::iterator p = list_names.begin();
       p != list_names.end();
       ++p)
    {
      Bvariable* bv =
        this->backend()->implicit_variable_reference(*p, *p, bt);
      Bexpression* bexpr = this->backend()->var_expression(bv, builtin_loc);
      bexpr = this->backend()->address_expression(bexpr, builtin_loc);

      indexes.push_back(i);
      vals.push_back(bexpr);
      i++;
    }
  Expression* len_expr = Expression::make_integer_ul(i, NULL, builtin_loc);
  Type* list_ptr_type = Type::make_pointer_type(list_type);
  Type* list_array_type = Type::make_array_type(list_ptr_type, len_expr);
  Btype* bat = list_array_type->get_backend(this);
  Bexpression* barray =
    this->backend()->array_constructor_expression(bat, indexes, vals,
                                                  builtin_loc);

  // Create a variable holding the list.
  std::string name = this->typelists_symbol();
  unsigned int flags = (Backend::variable_is_hidden
			| Backend::variable_is_constant);
  Bvariable* bv = this->backend()->implicit_variable(name, name, bat, flags,
                                                     0);
  this->backend()->implicit_variable_set_init(bv, name, bat, flags, barray);

  // Build the call in main package's init function.
  Translate_context context(this, NULL, NULL, NULL);
  Bexpression* bexpr = this->backend()->var_expression(bv, builtin_loc);
  bexpr = this->backend()->address_expression(bexpr, builtin_loc);
  Type* array_ptr_type = Type::make_pointer_type(list_array_type);
  Expression* expr = Expression::make_backend(bexpr, array_ptr_type,
                                              builtin_loc);
  expr = Runtime::make_call(Runtime::REGISTER_TYPE_DESCRIPTORS,
                            builtin_loc, 2, len_expr->copy(), expr);
  Bexpression* bcall = expr->get_backend(&context);
  init_stmts.push_back(this->backend()->expression_statement(init_bfn,
                                                             bcall));
}

// Build the decl for the initialization function.

Named_object*
Gogo::initialization_function_decl()
{
  std::string name = this->get_init_fn_name();
  Location loc = this->package_->location();

  Function_type* fntype = Type::make_function_type(NULL, NULL, NULL, loc);
  Function* initfn = new Function(fntype, NULL, NULL, loc);
  return Named_object::make_function(name, NULL, initfn);
}

// Create the magic initialization function.  CODE_STMT is the
// code that it needs to run.

Named_object*
Gogo::create_initialization_function(Named_object* initfn,
				     Bstatement* code_stmt)
{
  // Make sure that we thought we needed an initialization function,
  // as otherwise we will not have reported it in the export data.
  go_assert(this->is_main_package() || this->need_init_fn_);

  if (initfn == NULL)
    initfn = this->initialization_function_decl();

  // Bind the initialization function code to a block.
  Bfunction* fndecl = initfn->func_value()->get_or_make_decl(this, initfn);
  Location pkg_loc = this->package_->location();
  std::vector<Bvariable*> vars;
  this->backend()->block(fndecl, NULL, vars, pkg_loc, pkg_loc);

  if (!this->backend()->function_set_body(fndecl, code_stmt))
    {
      go_assert(saw_errors());
      return NULL;
    }
  return initfn;
}

// Given an expression, collect all the global variables defined in
// this package that it references.

class Find_vars : public Traverse
{
 private:
  // The list of variables we accumulate.
  typedef Unordered_set(Named_object*) Vars;

  // A hash table we use to avoid looping.  The index is a
  // Named_object* or a Temporary_statement*.  We only look through
  // objects defined in this package.
  typedef Unordered_set(const void*) Seen_objects;

 public:
  Find_vars()
    : Traverse(traverse_expressions | traverse_statements),
      vars_(), seen_objects_(), lhs_is_ref_(false)
  { }

  // An iterator through the variables found, after the traversal.
  typedef Vars::const_iterator const_iterator;

  const_iterator
  begin() const
  { return this->vars_.begin(); }

  const_iterator
  end() const
  { return this->vars_.end(); }

  int
  expression(Expression**);

  int
  statement(Block*, size_t* index, Statement*);

 private:
  // Accumulated variables.
  Vars vars_;
  // Objects we have already seen.
  Seen_objects seen_objects_;
  // Whether an assignment to a variable counts as a reference.
  bool lhs_is_ref_;
};

// Collect global variables referenced by EXPR.  Look through function
// calls and variable initializations.

int
Find_vars::expression(Expression** pexpr)
{
  Expression* e = *pexpr;

  Var_expression* ve = e->var_expression();
  if (ve != NULL)
    {
      Named_object* v = ve->named_object();
      if (!v->is_variable() || v->package() != NULL)
	{
	  // This is a result parameter or a variable defined in a
	  // different package.  Either way we don't care about it.
	  return TRAVERSE_CONTINUE;
	}

      std::pair<Seen_objects::iterator, bool> ins =
	this->seen_objects_.insert(v);
      if (!ins.second)
	{
	  // We've seen this variable before.
	  return TRAVERSE_CONTINUE;
	}

      if (v->var_value()->is_global())
	this->vars_.insert(v);

      Expression* init = v->var_value()->init();
      if (init != NULL)
	{
	  if (Expression::traverse(&init, this) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }

  // We traverse the code of any function or bound method we see.  Note that
  // this means that we will traverse the code of a function or bound method
  // whose address is taken even if it is not called.
  Func_expression* fe = e->func_expression();
  Bound_method_expression* bme = e->bound_method_expression();
  if (fe != NULL || bme != NULL)
    {
      const Named_object* f = fe != NULL ? fe->named_object() : bme->function();
      if (f->is_function() && f->package() == NULL)
	{
	  std::pair<Seen_objects::iterator, bool> ins =
	    this->seen_objects_.insert(f);
	  if (ins.second)
	    {
	      // This is the first time we have seen this name.
	      bool hold = this->lhs_is_ref_;
	      this->lhs_is_ref_ = true;
	      int r = f->func_value()->block()->traverse(this);
	      this->lhs_is_ref_ = hold;
	      if (r == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }

  Temporary_reference_expression* tre = e->temporary_reference_expression();
  if (tre != NULL)
    {
      Temporary_statement* ts = tre->statement();
      Expression* init = ts->init();
      if (init != NULL)
	{
	  std::pair<Seen_objects::iterator, bool> ins =
	    this->seen_objects_.insert(ts);
	  if (ins.second)
	    {
	      // This is the first time we have seen this temporary
	      // statement.
	      if (Expression::traverse(&init, this) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	}
    }

  return TRAVERSE_CONTINUE;
}

// Check a statement while searching for variables.  This is where we
// skip variables on the left hand side of assigments if appropriate.

int
Find_vars::statement(Block*, size_t*, Statement* s)
{
  if (this->lhs_is_ref_)
    return TRAVERSE_CONTINUE;
  Assignment_statement* as = s->assignment_statement();
  if (as == NULL)
    return TRAVERSE_CONTINUE;

  // Only traverse subexpressions of the LHS.
  if (as->lhs()->traverse_subexpressions(this) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;

  Expression* rhs = as->rhs();
  if (Expression::traverse(&rhs, this) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;

  return TRAVERSE_SKIP_COMPONENTS;
}

// Return true if EXPR, PREINIT, or DEP refers to VAR.

static bool
expression_requires(Expression* expr, Block* preinit, Named_object* dep,
		    Named_object* var)
{
  Find_vars find_vars;
  if (expr != NULL)
    Expression::traverse(&expr, &find_vars);
  if (preinit != NULL)
    preinit->traverse(&find_vars);
  if (dep != NULL)
    {
      Expression* init = dep->var_value()->init();
      if (init != NULL)
	Expression::traverse(&init, &find_vars);
      if (dep->var_value()->has_pre_init())
	dep->var_value()->preinit()->traverse(&find_vars);
    }

  for (Find_vars::const_iterator p = find_vars.begin();
       p != find_vars.end();
       ++p)
    {
      if (*p == var)
	return true;
    }
  return false;
}

// Sort variable initializations.  If the initialization expression
// for variable A refers directly or indirectly to the initialization
// expression for variable B, then we must initialize B before A.

class Var_init
{
 public:
  Var_init()
    : var_(NULL), init_(NULL), dep_count_(0)
  { }

  Var_init(Named_object* var, Bstatement* init)
    : var_(var), init_(init), dep_count_(0)
  { }

  // Return the variable.
  Named_object*
  var() const
  { return this->var_; }

  // Return the initialization expression.
  Bstatement*
  init() const
  { return this->init_; }

  // Return the number of remaining dependencies.
  size_t
  dep_count() const
  { return this->dep_count_; }

  // Increment the number of dependencies.
  void
  add_dependency()
  { ++this->dep_count_; }

  // Decrement the number of dependencies.
  void
  remove_dependency()
  { --this->dep_count_; }

 private:
  // The variable being initialized.
  Named_object* var_;
  // The backend initialization statement.
  Bstatement* init_;
  // The number of initializations this is dependent on.  A variable
  // initialization should not be emitted if any of its dependencies
  // have not yet been resolved.
  size_t dep_count_;
};

// For comparing Var_init keys in a map.

inline bool
operator<(const Var_init& v1, const Var_init& v2)
{ return v1.var()->name() < v2.var()->name(); }

typedef std::list<Var_init> Var_inits;

// Sort the variable initializations.  The rule we follow is that we
// emit them in the order they appear in the array, except that if the
// initialization expression for a variable V1 depends upon another
// variable V2 then we initialize V1 after V2.

static void
sort_var_inits(Var_inits* var_inits)
{
  if (var_inits->empty())
    return;

  std::map<Named_object*, Var_init*> var_to_init;

  // A mapping from a variable initialization to a set of
  // variable initializations that depend on it.
  typedef std::map<Var_init, std::set<Var_init*> > Init_deps;
  Init_deps init_deps;
  bool init_loop = false;

  // Map from variable to Var_init.
  for (Var_inits::iterator pvar = var_inits->begin();
       pvar != var_inits->end();
       ++pvar)
    {
      Named_object* var = pvar->var();
      var_to_init[var] = &*pvar;
    }

  // Add dependencies to init_deps, and check for cycles.
  for (Var_inits::iterator pvar = var_inits->begin();
       pvar != var_inits->end();
       ++pvar)
    {
      Named_object* var = pvar->var();

      const std::vector<Named_object*>* refs =
	pvar->var()->var_value()->init_refs();
      if (refs == NULL)
	continue;
      for (std::vector<Named_object*>::const_iterator pdep = refs->begin();
	   pdep != refs->end();
	   ++pdep)
	{
	  Named_object* dep = *pdep;
	  if (var == dep)
	    {
	      // This is a reference from a variable to itself.
	      go_error_at(var->location(),
			  ("initialization expression for %qs "
			   "depends upon itself"),
			  var->message_name().c_str());
	      continue;
	    }

	  Var_init* dep_init = var_to_init[dep];
	  if (dep_init == NULL)
	    {
	      // This is a dependency on some variable that doesn't
	      // have an initializer, so for purposes of
	      // initialization ordering this is irrelevant.
	      continue;
	    }

	  init_deps[*dep_init].insert(&(*pvar));
	  pvar->add_dependency();

	  // Check for cycles.
	  const std::vector<Named_object*>* deprefs =
	    dep_init->var()->var_value()->init_refs();
	  if (deprefs == NULL)
	    continue;
	  for (std::vector<Named_object*>::const_iterator pdepdep =
		 deprefs->begin();
	       pdepdep != deprefs->end();
	       ++pdepdep)
	    {
	      if (*pdepdep == var)
		{
		  go_error_at(var->location(),
			      ("initialization expressions for %qs and "
			       "%qs depend upon each other"),
			      var->message_name().c_str(),
			      dep->message_name().c_str());
		  go_inform(dep->location(), "%qs defined here",
			    dep->message_name().c_str());
		  init_loop = true;
		  break;
		}
	    }
	}
    }

  var_to_init.clear();

  // If there are no dependencies then the declaration order is sorted.
  if (!init_deps.empty() && !init_loop)
    {
      // Otherwise, sort variable initializations by emitting all variables with
      // no dependencies in declaration order. VAR_INITS is already in
      // declaration order.
      Var_inits ready;
      while (!var_inits->empty())
	{
	  Var_inits::iterator v1;;
	  for (v1 = var_inits->begin(); v1 != var_inits->end(); ++v1)
	    {
	      if (v1->dep_count() == 0)
		break;
	    }
	  go_assert(v1 != var_inits->end());

	  // V1 either has no dependencies or its dependencies have already
	  // been emitted, add it to READY next.  When V1 is emitted, remove
	  // a dependency from each V that depends on V1.
	  ready.splice(ready.end(), *var_inits, v1);

	  Init_deps::iterator p1 = init_deps.find(*v1);
	  if (p1 != init_deps.end())
	    {
	      std::set<Var_init*> resolved = p1->second;
	      for (std::set<Var_init*>::iterator pv = resolved.begin();
		   pv != resolved.end();
		   ++pv)
		(*pv)->remove_dependency();
	      init_deps.erase(p1);
	    }
	}
      var_inits->swap(ready);
      go_assert(init_deps.empty());
    }
}

// Give an error if the initialization expression for VAR depends on
// itself.  We only check if INIT is not NULL and there is no
// dependency; when INIT is NULL, it means that PREINIT sets VAR,
// which we will interpret as a loop.

void
Gogo::check_self_dep(Named_object* var)
{
  Expression* init = var->var_value()->init();
  Block* preinit = var->var_value()->preinit();
  Named_object* dep = this->var_depends_on(var->var_value());
  if (init != NULL
      && dep == NULL
      && expression_requires(init, preinit, NULL, var))
    go_error_at(var->location(),
		"initialization expression for %qs depends upon itself",
		var->message_name().c_str());
}

// Write out the global definitions.

void
Gogo::write_globals()
{
  this->build_interface_method_tables();

  Bindings* bindings = this->current_bindings();

  for (Bindings::const_declarations_iterator p = bindings->begin_declarations();
       p != bindings->end_declarations();
       ++p)
    {
      // If any function declarations needed a descriptor, make sure
      // we build it.
      Named_object* no = p->second;
      if (no->is_function_declaration())
	no->func_declaration_value()->build_backend_descriptor(this);
    }

  // Lists of globally declared types, variables, constants, and functions
  // that must be defined.
  std::vector<Btype*> type_decls;
  std::vector<Bvariable*> var_decls;
  std::vector<Bexpression*> const_decls;
  std::vector<Bfunction*> func_decls;

  // The init function declaration and associated Bfunction, if necessary.
  Named_object* init_fndecl = NULL;
  Bfunction* init_bfn = NULL;

  std::vector<Bstatement*> init_stmts;
  std::vector<Bstatement*> var_init_stmts;

  if (this->is_main_package())
    {
      init_fndecl = this->initialization_function_decl();
      init_bfn = init_fndecl->func_value()->get_or_make_decl(this, init_fndecl);
    }

  // A list of variable initializations.
  Var_inits var_inits;

  // A list of variables which need to be registered with the garbage
  // collector.
  size_t count_definitions = bindings->size_definitions();
  std::vector<Named_object*> var_gc;
  var_gc.reserve(count_definitions);

  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p)
    {
      Named_object* no = *p;
      go_assert(!no->is_type_declaration() && !no->is_function_declaration());

      // There is nothing to do for a package.
      if (no->is_package())
        continue;

      // There is nothing to do for an object which was imported from
      // a different package into the global scope.
      if (no->package() != NULL)
        continue;

      // Skip blank named functions and constants.
      if ((no->is_function() && no->func_value()->is_sink())
	  || (no->is_const() && no->const_value()->is_sink()))
        continue;

      // Skip global sink variables with static initializers.  With
      // non-static initializers we have to evaluate for side effects,
      // and we wind up initializing a dummy variable.  That is not
      // ideal but it works and it's a rare case.
      if (no->is_variable()
	  && no->var_value()->is_global_sink()
	  && !no->var_value()->has_pre_init()
	  && (no->var_value()->init() == NULL
	      || no->var_value()->init()->is_static_initializer()))
	continue;

      // There is nothing useful we can output for constants which
      // have ideal or non-integral type.
      if (no->is_const())
        {
          Type* type = no->const_value()->type();
          if (type == NULL)
            type = no->const_value()->expr()->type();
          if (type->is_abstract() || !type->is_numeric_type())
            continue;
        }

      if (!no->is_variable())
        no->get_backend(this, const_decls, type_decls, func_decls);
      else
	{
          Variable* var = no->var_value();
	  Bvariable* bvar = no->get_backend_variable(this, NULL);
          var_decls.push_back(bvar);

	  // Check for a sink variable, which may be used to run an
	  // initializer purely for its side effects.
	  bool is_sink = no->name()[0] == '_' && no->name()[1] == '.';

          Bstatement* var_init_stmt = NULL;
	  if (!var->has_pre_init())
	    {
              // If the backend representation of the variable initializer is
              // constant, we can just set the initial value using
              // global_var_set_init instead of during the init() function.
              // The initializer is constant if it is the zero-value of the
              // variable's type or if the initial value is an immutable value
              // that is not copied to the heap.
              bool is_static_initializer = false;
              if (var->init() == NULL)
                is_static_initializer = true;
              else
                {
                  Type* var_type = var->type();
                  Expression* init = var->init();
                  Expression* init_cast =
                      Expression::make_cast(var_type, init, var->location());
                  is_static_initializer = init_cast->is_static_initializer();
                }

	      // Non-constant variable initializations might need to create
	      // temporary variables, which will need the initialization
	      // function as context.
	      Named_object* var_init_fn;
	      if (is_static_initializer)
		var_init_fn = NULL;
	      else
		{
		  if (init_fndecl == NULL)
                    {
                      init_fndecl = this->initialization_function_decl();
                      Function* func = init_fndecl->func_value();
                      init_bfn = func->get_or_make_decl(this, init_fndecl);
                    }
		  var_init_fn = init_fndecl;
		}
              Bexpression* var_binit = var->get_init(this, var_init_fn);

              if (var_binit == NULL)
		;
	      else if (is_static_initializer)
		{
		  if (expression_requires(var->init(), NULL,
					  this->var_depends_on(var), no))
		    go_error_at(no->location(),
				"initialization expression for %qs depends "
				"upon itself",
				no->message_name().c_str());
		  this->backend()->global_variable_set_init(bvar, var_binit);
		}
	      else if (is_sink)
	        var_init_stmt =
                    this->backend()->expression_statement(init_bfn, var_binit);
	      else
                {
                  Location loc = var->location();
                  Bexpression* var_expr =
                      this->backend()->var_expression(bvar, loc);
                  var_init_stmt =
                      this->backend()->assignment_statement(init_bfn, var_expr,
                                                            var_binit, loc);
                }
	    }
	  else
	    {
	      // We are going to create temporary variables which
	      // means that we need an fndecl.
              if (init_fndecl == NULL)
		init_fndecl = this->initialization_function_decl();

	      Bvariable* var_decl = is_sink ? NULL : bvar;
	      var_init_stmt = var->get_init_block(this, init_fndecl, var_decl);
	    }

	  if (var_init_stmt != NULL)
	    {
	      if (var->init() == NULL && !var->has_pre_init())
                var_init_stmts.push_back(var_init_stmt);
	      else
                var_inits.push_back(Var_init(no, var_init_stmt));
	    }
	  else if (this->var_depends_on(var) != NULL)
	    {
	      // This variable is initialized from something that is
	      // not in its init or preinit.  This variable needs to
	      // participate in dependency analysis sorting, in case
	      // some other variable depends on this one.
              Btype* btype = no->var_value()->type()->get_backend(this);
              Bexpression* zero = this->backend()->zero_expression(btype);
              Bstatement* zero_stmt =
                  this->backend()->expression_statement(init_bfn, zero);
	      var_inits.push_back(Var_init(no, zero_stmt));
	    }

	  // Collect a list of all global variables with pointers,
	  // to register them for the garbage collector.
	  if (!is_sink && var->type()->has_pointer())
	    {
	      // Avoid putting runtime.gcRoots itself on the list.
	      if (this->compiling_runtime()
		  && this->package_name() == "runtime"
		  && (Gogo::unpack_hidden_name(no->name()) == "gcRoots"
                   || Gogo::unpack_hidden_name(no->name()) == "gcRootsIndex"))
		;
	      else
		var_gc.push_back(no);
	    }
	}
    }

  // Output inline functions, which are in different packages.
  for (std::vector<Named_object*>::const_iterator p =
	 this->imported_inline_functions_.begin();
       p != this->imported_inline_functions_.end();
       ++p)
    (*p)->get_backend(this, const_decls, type_decls, func_decls);

  // Build the list of type descriptors.
  this->build_type_descriptor_list();

  if (this->is_main_package())
    {
      // Register the type descriptor lists, so that at run time
      // the reflect package can find compiler-created types, and
      // deduplicate if the same type is created with reflection.
      // This needs to be done before calling any package's init
      // function, as it may create type through reflection.
      this->register_type_descriptors(init_stmts, init_bfn);

      // Initialize imported packages.
      this->init_imports(init_stmts, init_bfn);
    }

  // Register global variables with the garbage collector.
  this->register_gc_vars(var_gc, init_stmts, init_bfn);

  // Simple variable initializations, after all variables are
  // registered.
  init_stmts.push_back(this->backend()->statement_list(var_init_stmts));

  // Complete variable initializations, first sorting them into a
  // workable order.
  if (!var_inits.empty())
    {
      sort_var_inits(&var_inits);
      for (Var_inits::const_iterator p = var_inits.begin();
           p != var_inits.end();
           ++p)
        init_stmts.push_back(p->init());
    }

  // After all the variables are initialized, call the init
  // functions if there are any.  Init functions take no arguments, so
  // we pass in EMPTY_ARGS to call them.
  std::vector<Bexpression*> empty_args;
  for (std::vector<Named_object*>::const_iterator p =
           this->init_functions_.begin();
       p != this->init_functions_.end();
       ++p)
    {
      Location func_loc = (*p)->location();
      Function* func = (*p)->func_value();
      Bfunction* initfn = func->get_or_make_decl(this, *p);
      Bexpression* func_code =
          this->backend()->function_code_expression(initfn, func_loc);
      Bexpression* call = this->backend()->call_expression(init_bfn, func_code,
                                                           empty_args,
							   NULL, func_loc);
      Bstatement* ist = this->backend()->expression_statement(init_bfn, call);
      init_stmts.push_back(ist);
    }

  // Set up a magic function to do all the initialization actions.
  // This will be called if this package is imported.
  Bstatement* init_fncode = this->backend()->statement_list(init_stmts);
  if (this->need_init_fn_ || this->is_main_package())
    {
      init_fndecl =
	this->create_initialization_function(init_fndecl, init_fncode);
      if (init_fndecl != NULL)
	func_decls.push_back(init_fndecl->func_value()->get_decl());
    }

  // We should not have seen any new bindings created during the conversion.
  go_assert(count_definitions == this->current_bindings()->size_definitions());

  // Define all globally declared values.
  if (!saw_errors())
    this->backend()->write_global_definitions(type_decls, const_decls,
					      func_decls, var_decls);
}

// Return the current block.

Block*
Gogo::current_block()
{
  if (this->functions_.empty())
    return NULL;
  else
    return this->functions_.back().blocks.back();
}

// Look up a name in the current binding contour.  If PFUNCTION is not
// NULL, set it to the function in which the name is defined, or NULL
// if the name is defined in global scope.

Named_object*
Gogo::lookup(const std::string& name, Named_object** pfunction) const
{
  if (pfunction != NULL)
    *pfunction = NULL;

  if (Gogo::is_sink_name(name))
    return Named_object::make_sink();

  for (Open_functions::const_reverse_iterator p = this->functions_.rbegin();
       p != this->functions_.rend();
       ++p)
    {
      Named_object* ret = p->blocks.back()->bindings()->lookup(name);
      if (ret != NULL)
	{
	  if (pfunction != NULL)
	    *pfunction = p->function;
	  return ret;
	}
    }

  if (this->package_ != NULL)
    {
      Named_object* ret = this->package_->bindings()->lookup(name);
      if (ret != NULL)
	{
	  if (ret->package() != NULL)
            {
              std::string dot_alias = "." + ret->package()->package_name();
              ret->package()->note_usage(dot_alias);
            }
	  return ret;
	}
    }

  // We do not look in the global namespace.  If we did, the global
  // namespace would effectively hide names which were defined in
  // package scope which we have not yet seen.  Instead,
  // define_global_names is called after parsing is over to connect
  // undefined names at package scope with names defined at global
  // scope.

  return NULL;
}

// Look up a name in the current block, without searching enclosing
// blocks.

Named_object*
Gogo::lookup_in_block(const std::string& name) const
{
  go_assert(!this->functions_.empty());
  go_assert(!this->functions_.back().blocks.empty());
  return this->functions_.back().blocks.back()->bindings()->lookup_local(name);
}

// Look up a name in the global namespace.

Named_object*
Gogo::lookup_global(const char* name) const
{
  return this->globals_->lookup(name);
}

// Add an imported package.

Package*
Gogo::add_imported_package(const std::string& real_name,
			   const std::string& alias_arg,
			   bool is_alias_exported,
			   const std::string& pkgpath,
			   const std::string& pkgpath_symbol,
			   Location location,
			   bool* padd_to_globals)
{
  Package* ret = this->register_package(pkgpath, pkgpath_symbol, location);
  ret->set_package_name(real_name, location);

  *padd_to_globals = false;

  if (alias_arg == "_")
    ;
  else if (alias_arg == ".")
    {
      *padd_to_globals = true;
      std::string dot_alias = "." + real_name;
      ret->add_alias(dot_alias, location);
    }
  else
    {
      std::string alias = alias_arg;
      if (alias.empty())
	{
	  alias = real_name;
	  is_alias_exported = Lex::is_exported_name(alias);
	}
      ret->add_alias(alias, location);
      alias = this->pack_hidden_name(alias, is_alias_exported);
      Named_object* no = this->package_->bindings()->add_package(alias, ret);
      if (!no->is_package())
	return NULL;
    }

  return ret;
}

// Register a package.  This package may or may not be imported.  This
// returns the Package structure for the package, creating if it
// necessary.  LOCATION is the location of the import statement that
// led us to see this package.  PKGPATH_SYMBOL is the symbol to use
// for names in the package; it may be the empty string, in which case
// we either get it later or make a guess when we need it.

Package*
Gogo::register_package(const std::string& pkgpath,
		       const std::string& pkgpath_symbol, Location location)
{
  Package* package = NULL;
  std::pair<Packages::iterator, bool> ins =
    this->packages_.insert(std::make_pair(pkgpath, package));
  if (!ins.second)
    {
      // We have seen this package name before.
      package = ins.first->second;
      go_assert(package != NULL && package->pkgpath() == pkgpath);
      if (!pkgpath_symbol.empty())
	package->set_pkgpath_symbol(pkgpath_symbol);
      if (Linemap::is_unknown_location(package->location()))
	package->set_location(location);
    }
  else
    {
      // First time we have seen this package name.
      package = new Package(pkgpath, pkgpath_symbol, location);
      go_assert(ins.first->second == NULL);
      ins.first->second = package;
    }

  return package;
}

// Return the pkgpath symbol for a package, given the pkgpath.

std::string
Gogo::pkgpath_symbol_for_package(const std::string& pkgpath)
{
  Packages::iterator p = this->packages_.find(pkgpath);
  go_assert(p != this->packages_.end());
  return p->second->pkgpath_symbol();
}

// Start compiling a function.

Named_object*
Gogo::start_function(const std::string& name, Function_type* type,
		     bool add_method_to_type, Location location)
{
  bool at_top_level = this->functions_.empty();

  Block* block = new Block(NULL, location);

  Named_object* enclosing = (at_top_level
			 ? NULL
			 : this->functions_.back().function);

  Function* function = new Function(type, enclosing, block, location);

  if (type->is_method())
    {
      const Typed_identifier* receiver = type->receiver();
      Variable* this_param = new Variable(receiver->type(), NULL, false,
					  true, true, location);
      std::string rname = receiver->name();
      unsigned rcounter = 0;

      // We need to give a nameless receiver parameter a synthesized name to
      // avoid having it clash with some other nameless param. FIXME.
      Gogo::rename_if_empty(&rname, "r", &rcounter);

      block->bindings()->add_variable(rname, NULL, this_param);
    }

  const Typed_identifier_list* parameters = type->parameters();
  bool is_varargs = type->is_varargs();
  unsigned pcounter = 0;
  if (parameters != NULL)
    {
      for (Typed_identifier_list::const_iterator p = parameters->begin();
	   p != parameters->end();
	   ++p)
	{
	  Variable* param = new Variable(p->type(), NULL, false, true, false,
					 p->location());
	  if (is_varargs && p + 1 == parameters->end())
	    param->set_is_varargs_parameter();

	  std::string pname = p->name();

          // We need to give each nameless parameter a non-empty name to avoid
          // having it clash with some other nameless param. FIXME.
          Gogo::rename_if_empty(&pname, "p", &pcounter);

	  block->bindings()->add_variable(pname, NULL, param);
	}
    }

  function->create_result_variables(this);

  const std::string* pname;
  std::string nested_name;
  bool is_init = false;
  if (Gogo::unpack_hidden_name(name) == "init" && !type->is_method())
    {
      if ((type->parameters() != NULL && !type->parameters()->empty())
	  || (type->results() != NULL && !type->results()->empty()))
	go_error_at(location,
		    "func init must have no arguments and no return values");
      // There can be multiple "init" functions, so give them each a
      // different name.
      nested_name = this->init_function_name();
      pname = &nested_name;
      is_init = true;
    }
  else if (!name.empty())
    pname = &name;
  else
    {
      // Invent a name for a nested function.
      nested_name = this->nested_function_name(enclosing);
      pname = &nested_name;
    }

  Named_object* ret;
  if (Gogo::is_sink_name(*pname))
    {
      std::string sname(this->sink_function_name());
      ret = Named_object::make_function(sname, NULL, function);
      ret->func_value()->set_is_sink();

      if (!type->is_method())
	ret = this->package_->bindings()->add_named_object(ret);
      else if (add_method_to_type)
	{
	  // We should report errors even for sink methods.
	  Type* rtype = type->receiver()->type();
	  // Avoid points_to and deref to avoid getting an error if
	  // the type is not yet defined.
	  if (rtype->classification() == Type::TYPE_POINTER)
	    rtype = rtype->points_to();
	  while (rtype->named_type() != NULL
		 && rtype->named_type()->is_alias())
	    rtype = rtype->named_type()->real_type()->forwarded();
	  if (rtype->is_error_type())
	    ;
	  else if (rtype->named_type() != NULL)
	    {
	      if (rtype->named_type()->named_object()->package() != NULL)
		go_error_at(type->receiver()->location(),
			    "may not define methods on non-local type");
	    }
	  else if (rtype->forward_declaration_type() != NULL)
	    {
	      // Go ahead and add the method in case we need to report
	      // an error when we see the definition.
	      rtype->forward_declaration_type()->add_existing_method(ret);
	    }
	  else
	    go_error_at(type->receiver()->location(),
			("invalid receiver type "
			 "(receiver must be a named type)"));
	}
    }
  else if (!type->is_method())
    {
      ret = this->package_->bindings()->add_function(*pname, NULL, function);
      if (!ret->is_function() || ret->func_value() != function)
	{
	  // Redefinition error.  Invent a name to avoid knockon
	  // errors.
	  std::string rname(this->redefined_function_name());
	  ret = this->package_->bindings()->add_function(rname, NULL, function);
	}
    }
  else
    {
      if (!add_method_to_type)
	ret = Named_object::make_function(name, NULL, function);
      else
	{
	  go_assert(at_top_level);
	  Type* rtype = type->receiver()->type();

	  while (rtype->named_type() != NULL
		 && rtype->named_type()->is_alias())
	    rtype = rtype->named_type()->real_type()->forwarded();

	  // We want to look through the pointer created by the
	  // parser, without getting an error if the type is not yet
	  // defined.
	  if (rtype->classification() == Type::TYPE_POINTER)
	    rtype = rtype->points_to();

	  while (rtype->named_type() != NULL
		 && rtype->named_type()->is_alias())
	    rtype = rtype->named_type()->real_type()->forwarded();

	  if (rtype->is_error_type())
	    ret = Named_object::make_function(name, NULL, function);
	  else if (rtype->named_type() != NULL)
	    {
	      if (rtype->named_type()->named_object()->package() != NULL)
		{
		  go_error_at(type->receiver()->location(),
			      "may not define methods on non-local type");
		  ret = Named_object::make_function(name, NULL, function);
		}
	      else
		{
		  ret = rtype->named_type()->add_method(name, function);
		  if (!ret->is_function())
		    {
		      // Redefinition error.
		      ret = Named_object::make_function(name, NULL, function);
		    }
		}
	    }
	  else if (rtype->forward_declaration_type() != NULL)
	    {
	      Named_object* type_no =
		rtype->forward_declaration_type()->named_object();
	      if (type_no->is_unknown())
		{
		  // If we are seeing methods it really must be a
		  // type.  Declare it as such.  An alternative would
		  // be to support lists of methods for unknown
		  // expressions.  Either way the error messages if
		  // this is not a type are going to get confusing.
		  Named_object* declared =
		    this->declare_package_type(type_no->name(),
					       type_no->location());
		  go_assert(declared
			     == type_no->unknown_value()->real_named_object());
		}
	      ret = rtype->forward_declaration_type()->add_method(name,
								  function);
	    }
	  else
            {
	      go_error_at(type->receiver()->location(),
			  ("invalid receiver type (receiver must "
			   "be a named type)"));
              ret = Named_object::make_function(name, NULL, function);
            }
	}
      this->package_->bindings()->add_method(ret);
    }

  this->functions_.resize(this->functions_.size() + 1);
  Open_function& of(this->functions_.back());
  of.function = ret;
  of.blocks.push_back(block);

  if (is_init)
    {
      this->init_functions_.push_back(ret);
      this->need_init_fn_ = true;
    }

  return ret;
}

// Finish compiling a function.

void
Gogo::finish_function(Location location)
{
  this->finish_block(location);
  go_assert(this->functions_.back().blocks.empty());
  this->functions_.pop_back();
}

// Return the current function.

Named_object*
Gogo::current_function() const
{
  go_assert(!this->functions_.empty());
  return this->functions_.back().function;
}

// Start a new block.

void
Gogo::start_block(Location location)
{
  go_assert(!this->functions_.empty());
  Block* block = new Block(this->current_block(), location);
  this->functions_.back().blocks.push_back(block);
}

// Finish a block.

Block*
Gogo::finish_block(Location location)
{
  go_assert(!this->functions_.empty());
  go_assert(!this->functions_.back().blocks.empty());
  Block* block = this->functions_.back().blocks.back();
  this->functions_.back().blocks.pop_back();
  block->set_end_location(location);
  return block;
}

// Add an erroneous name.

Named_object*
Gogo::add_erroneous_name(const std::string& name)
{
  return this->package_->bindings()->add_erroneous_name(name);
}

// Add an unknown name.

Named_object*
Gogo::add_unknown_name(const std::string& name, Location location)
{
  return this->package_->bindings()->add_unknown_name(name, location);
}

// Declare a function.

Named_object*
Gogo::declare_function(const std::string& name, Function_type* type,
		       Location location)
{
  if (!type->is_method())
    return this->current_bindings()->add_function_declaration(name, NULL, type,
							      location);
  else
    {
      // We don't bother to add this to the list of global
      // declarations.
      Type* rtype = type->receiver()->type();

      while (rtype->named_type() != NULL
	  && rtype->named_type()->is_alias())
	rtype = rtype->named_type()->real_type()->forwarded();

      // We want to look through the pointer created by the
      // parser, without getting an error if the type is not yet
      // defined.
      if (rtype->classification() == Type::TYPE_POINTER)
	rtype = rtype->points_to();

      while (rtype->named_type() != NULL
	  && rtype->named_type()->is_alias())
	rtype = rtype->named_type()->real_type()->forwarded();

      if (rtype->is_error_type())
	return NULL;
      else if (rtype->named_type() != NULL)
	return rtype->named_type()->add_method_declaration(name, NULL, type,
							   location);
      else if (rtype->forward_declaration_type() != NULL)
	{
	  Forward_declaration_type* ftype = rtype->forward_declaration_type();
	  return ftype->add_method_declaration(name, NULL, type, location);
	}
      else
        {
	  go_error_at(type->receiver()->location(),
		      "invalid receiver type (receiver must be a named type)");
          return Named_object::make_erroneous_name(name);
        }
    }
}

// Add a label definition.

Label*
Gogo::add_label_definition(const std::string& label_name,
			   Location location)
{
  go_assert(!this->functions_.empty());
  Function* func = this->functions_.back().function->func_value();
  Label* label = func->add_label_definition(this, label_name, location);
  this->add_statement(Statement::make_label_statement(label, location));
  return label;
}

// Add a label reference.

Label*
Gogo::add_label_reference(const std::string& label_name,
			  Location location, bool issue_goto_errors)
{
  go_assert(!this->functions_.empty());
  Function* func = this->functions_.back().function->func_value();
  return func->add_label_reference(this, label_name, location,
				   issue_goto_errors);
}

// Return the current binding state.

Bindings_snapshot*
Gogo::bindings_snapshot(Location location)
{
  return new Bindings_snapshot(this->current_block(), location);
}

// Add a statement.

void
Gogo::add_statement(Statement* statement)
{
  go_assert(!this->functions_.empty()
	     && !this->functions_.back().blocks.empty());
  this->functions_.back().blocks.back()->add_statement(statement);
}

// Add a block.

void
Gogo::add_block(Block* block, Location location)
{
  go_assert(!this->functions_.empty()
	     && !this->functions_.back().blocks.empty());
  Statement* statement = Statement::make_block_statement(block, location);
  this->functions_.back().blocks.back()->add_statement(statement);
}

// Add a constant.

Named_object*
Gogo::add_constant(const Typed_identifier& tid, Expression* expr,
		   int iota_value)
{
  return this->current_bindings()->add_constant(tid, NULL, expr, iota_value);
}

// Add a type.

void
Gogo::add_type(const std::string& name, Type* type, Location location)
{
  Named_object* no = this->current_bindings()->add_type(name, NULL, type,
							location);
  if (!this->in_global_scope() && no->is_type())
    {
      Named_object* f = this->functions_.back().function;
      unsigned int index;
      if (f->is_function())
	index = f->func_value()->new_local_type_index();
      else
	index = 0;
      no->type_value()->set_in_function(f, index);
    }
}

// Add a named type.

void
Gogo::add_named_type(Named_type* type)
{
  go_assert(this->in_global_scope());
  this->current_bindings()->add_named_type(type);
}

// Declare a type.

Named_object*
Gogo::declare_type(const std::string& name, Location location)
{
  Bindings* bindings = this->current_bindings();
  Named_object* no = bindings->add_type_declaration(name, NULL, location);
  if (!this->in_global_scope() && no->is_type_declaration())
    {
      Named_object* f = this->functions_.back().function;
      unsigned int index;
      if (f->is_function())
	index = f->func_value()->new_local_type_index();
      else
	index = 0;
      no->type_declaration_value()->set_in_function(f, index);
    }
  return no;
}

// Declare a type at the package level.

Named_object*
Gogo::declare_package_type(const std::string& name, Location location)
{
  return this->package_->bindings()->add_type_declaration(name, NULL, location);
}

// Declare a function at the package level.

Named_object*
Gogo::declare_package_function(const std::string& name, Function_type* type,
			       Location location)
{
  return this->package_->bindings()->add_function_declaration(name, NULL, type,
							      location);
}

// Add a function declaration to the list of functions we may want to
// inline.

void
Gogo::add_imported_inlinable_function(Named_object* no)
{
  go_assert(no->is_function_declaration());
  Function_declaration* fd = no->func_declaration_value();
  if (fd->is_on_inlinable_list())
    return;
  this->imported_inlinable_functions_.push_back(no);
  fd->set_is_on_inlinable_list();
}

// Define a type which was already declared.

void
Gogo::define_type(Named_object* no, Named_type* type)
{
  this->current_bindings()->define_type(no, type);
}

// Add a variable.

Named_object*
Gogo::add_variable(const std::string& name, Variable* variable)
{
  Named_object* no = this->current_bindings()->add_variable(name, NULL,
							    variable);

  // In a function the middle-end wants to see a DECL_EXPR node.
  if (no != NULL
      && no->is_variable()
      && !no->var_value()->is_parameter()
      && !this->functions_.empty())
    this->add_statement(Statement::make_variable_declaration(no));

  return no;
}

void
Gogo::rename_if_empty(std::string* pname, const char* tag, unsigned* count)
{
  if (pname->empty() || Gogo::is_sink_name(*pname))
    {
      char buf[50];
      go_assert(strlen(tag) < 10);
      snprintf(buf, sizeof buf, "%s.%u", tag, *count);
      ++(*count);
      *pname = buf;
    }
}


// Add a sink--a reference to the blank identifier _.

Named_object*
Gogo::add_sink()
{
  return Named_object::make_sink();
}

// Add a named object for a dot import.

void
Gogo::add_dot_import_object(Named_object* no)
{
  // If the name already exists, then it was defined in some file seen
  // earlier.  If the earlier name is just a declaration, don't add
  // this name, because that will cause the previous declaration to
  // merge to this imported name, which should not happen.  Just add
  // this name to the list of file block names to get appropriate
  // errors if we see a later definition.
  Named_object* e = this->package_->bindings()->lookup(no->name());
  if (e != NULL && e->package() == NULL)
    {
      if (e->is_unknown())
	e = e->resolve();
      if (e->package() == NULL
	  && (e->is_type_declaration()
	      || e->is_function_declaration()
	      || e->is_unknown()))
	{
	  this->add_file_block_name(no->name(), no->location());
	  return;
	}
    }

  this->current_bindings()->add_named_object(no);
}

// Add a linkname.  This implements the go:linkname compiler directive.
// We only support this for functions and function declarations.

void
Gogo::add_linkname(const std::string& go_name, bool is_exported,
		   const std::string& ext_name, Location loc)
{
  Named_object* no =
    this->package_->bindings()->lookup(this->pack_hidden_name(go_name,
							      is_exported));
  if (no == NULL)
    go_error_at(loc, "%s is not defined", go_name.c_str());
  else if (no->is_function())
    {
      if (ext_name.empty())
	no->func_value()->set_is_exported_by_linkname();
      else
	no->func_value()->set_asm_name(ext_name);
    }
  else if (no->is_function_declaration())
    {
      if (ext_name.empty())
	go_error_at(loc,
		    ("%<//go:linkname%> missing external name "
		     "for declaration of %s"),
		    go_name.c_str());
      else
	no->func_declaration_value()->set_asm_name(ext_name);
    }
  else
    go_error_at(loc,
		("%s is not a function; "
		 "%<//go:linkname%> is only supported for functions"),
		go_name.c_str());
}

// Mark all local variables used.  This is used when some types of
// parse error occur.

void
Gogo::mark_locals_used()
{
  for (Open_functions::iterator pf = this->functions_.begin();
       pf != this->functions_.end();
       ++pf)
    {
      for (std::vector<Block*>::iterator pb = pf->blocks.begin();
	   pb != pf->blocks.end();
	   ++pb)
	(*pb)->bindings()->mark_locals_used();
    }
}

// Record that we've seen an interface type.

void
Gogo::record_interface_type(Interface_type* itype)
{
  this->interface_types_.push_back(itype);
}

// Define the global names.  We do this only after parsing all the
// input files, because the program might define the global names
// itself.

void
Gogo::define_global_names()
{
  if (this->is_main_package())
    {
      // Every Go program has to import the runtime package, so that
      // it is properly initialized.  We can't use
      // predeclared_location here as it will cause runtime functions
      // to appear to be builtin functions.
      this->import_package("runtime", "_", false, false,
			   this->package_->location());
    }

  for (Bindings::const_declarations_iterator p =
	 this->globals_->begin_declarations();
       p != this->globals_->end_declarations();
       ++p)
    {
      Named_object* global_no = p->second;
      std::string name(Gogo::pack_hidden_name(global_no->name(), false));
      Named_object* no = this->package_->bindings()->lookup(name);
      if (no == NULL)
	continue;
      no = no->resolve();
      if (no->is_type_declaration())
	{
	  if (global_no->is_type())
	    {
	      if (no->type_declaration_value()->has_methods())
		{
		  for (std::vector<Named_object*>::const_iterator pm =
			 no->type_declaration_value()->methods()->begin();
		       pm != no->type_declaration_value()->methods()->end();
		       pm++)
		    go_error_at((*pm)->location(),
				"may not define methods on non-local type");
		}
	      no->set_type_value(global_no->type_value());
	    }
	  else
	    {
	      go_error_at(no->location(), "expected type");
	      Type* errtype = Type::make_error_type();
	      Named_object* err =
                Named_object::make_type("erroneous_type", NULL, errtype,
                                        Linemap::predeclared_location());
	      no->set_type_value(err->type_value());
	    }
	}
      else if (no->is_unknown())
	no->unknown_value()->set_real_named_object(global_no);
    }

  // Give an error if any name is defined in both the package block
  // and the file block.  For example, this can happen if one file
  // imports "fmt" and another file defines a global variable fmt.
  for (Bindings::const_declarations_iterator p =
	 this->package_->bindings()->begin_declarations();
       p != this->package_->bindings()->end_declarations();
       ++p)
    {
      if (p->second->is_unknown()
	  && p->second->unknown_value()->real_named_object() == NULL)
	{
	  // No point in warning about an undefined name, as we will
	  // get other errors later anyhow.
	  continue;
	}
      File_block_names::const_iterator pf =
	this->file_block_names_.find(p->second->name());
      if (pf != this->file_block_names_.end())
	{
	  std::string n = p->second->message_name();
	  go_error_at(p->second->location(),
		      "%qs defined as both imported name and global name",
		      n.c_str());
	  go_inform(pf->second, "%qs imported here", n.c_str());
	}

      // No package scope identifier may be named "init".
      if (!p->second->is_function()
	  && Gogo::unpack_hidden_name(p->second->name()) == "init")
	{
	  go_error_at(p->second->location(),
		      "cannot declare init - must be func");
	}
    }
}

// Clear out names in file scope.

void
Gogo::clear_file_scope()
{
  this->package_->bindings()->clear_file_scope(this);

  // Warn about packages which were imported but not used.
  bool quiet = saw_errors();
  for (Packages::iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    {
      Package* package = p->second;
      if (package != this->package_ && !quiet)
        {
          for (Package::Aliases::const_iterator p1 = package->aliases().begin();
               p1 != package->aliases().end();
               ++p1)
            {
              if (!p1->second->used())
                {
                  // Give a more refined error message if the alias name is known.
                  std::string pkg_name = package->package_name();
                  if (p1->first != pkg_name && p1->first[0] != '.')
                    {
		      go_error_at(p1->second->location(),
				  "imported and not used: %s as %s",
				  Gogo::message_name(pkg_name).c_str(),
				  Gogo::message_name(p1->first).c_str());
                    }
                  else
		    go_error_at(p1->second->location(),
				"imported and not used: %s",
				Gogo::message_name(pkg_name).c_str());
                }
            }
        }
      package->clear_used();
    }

  this->current_file_imported_unsafe_ = false;
  this->current_file_imported_embed_ = false;
}

// Queue up a type-specific hash function for later writing.  These
// are written out in write_specific_type_functions, called after the
// parse tree is lowered.

void
Gogo::queue_hash_function(Type* type, int64_t size, Backend_name* bname,
			  Function_type* hash_fntype)
{
  go_assert(!this->specific_type_functions_are_written_);
  go_assert(!this->in_global_scope());
  Specific_type_function::Specific_type_function_kind kind =
    Specific_type_function::SPECIFIC_HASH;
  Specific_type_function* tsf = new Specific_type_function(type, NULL, size,
							   kind, bname,
							   hash_fntype);
  this->specific_type_functions_.push_back(tsf);
}

// Queue up a type-specific equal function for later writing.  These
// are written out in write_specific_type_functions, called after the
// parse tree is lowered.

void
Gogo::queue_equal_function(Type* type, Named_type* name, int64_t size,
			   Backend_name* bname, Function_type* equal_fntype)
{
  go_assert(!this->specific_type_functions_are_written_);
  go_assert(!this->in_global_scope());
  Specific_type_function::Specific_type_function_kind kind =
    Specific_type_function::SPECIFIC_EQUAL;
  Specific_type_function* tsf = new Specific_type_function(type, name, size,
							   kind, bname,
							   equal_fntype);
  this->specific_type_functions_.push_back(tsf);
}

// Look for types which need specific hash or equality functions.

class Specific_type_functions : public Traverse
{
 public:
  Specific_type_functions(Gogo* gogo)
    : Traverse(traverse_types),
      gogo_(gogo)
  { }

  int
  type(Type*);

 private:
  Gogo* gogo_;
};

int
Specific_type_functions::type(Type* t)
{
  switch (t->classification())
    {
    case Type::TYPE_NAMED:
      {
	Named_type* nt = t->named_type();
	if (nt->is_alias())
	  return TRAVERSE_CONTINUE;
	if (t->needs_specific_type_functions(this->gogo_))
	  t->equal_function(this->gogo_, nt, NULL);

	// If this is a struct type, we don't want to make functions
	// for the unnamed struct.
	Type* rt = nt->real_type();
	if (rt->struct_type() == NULL)
	  {
	    if (Type::traverse(rt, this) == TRAVERSE_EXIT)
	      return TRAVERSE_EXIT;
	  }
	else
	  {
	    // If this type is defined in another package, then we don't
	    // need to worry about the unexported fields.
	    bool is_defined_elsewhere = nt->named_object()->package() != NULL;
	    const Struct_field_list* fields = rt->struct_type()->fields();
	    for (Struct_field_list::const_iterator p = fields->begin();
		 p != fields->end();
		 ++p)
	      {
		if (is_defined_elsewhere
		    && Gogo::is_hidden_name(p->field_name()))
		  continue;
		if (Type::traverse(p->type(), this) == TRAVERSE_EXIT)
		  return TRAVERSE_EXIT;
	      }
	  }

	return TRAVERSE_SKIP_COMPONENTS;
      }

    case Type::TYPE_STRUCT:
    case Type::TYPE_ARRAY:
      if (t->needs_specific_type_functions(this->gogo_))
	t->equal_function(this->gogo_, NULL, NULL);
      break;

    case Type::TYPE_MAP:
      {
	Type* key_type = t->map_type()->key_type()->unalias();
	if (key_type->needs_specific_type_functions(this->gogo_))
	  key_type->hash_function(this->gogo_, NULL);
      }
      break;

    default:
      break;
    }

  return TRAVERSE_CONTINUE;
}

// Write out type specific functions.

void
Gogo::write_specific_type_functions()
{
  Specific_type_functions stf(this);
  this->traverse(&stf);

  while (!this->specific_type_functions_.empty())
    {
      Specific_type_function* tsf = this->specific_type_functions_.back();
      this->specific_type_functions_.pop_back();
      if (tsf->kind == Specific_type_function::SPECIFIC_HASH)
	tsf->type->write_hash_function(this, tsf->size, &tsf->bname,
				       tsf->fntype);
      else
	tsf->type->write_equal_function(this, tsf->name, tsf->size,
					&tsf->bname, tsf->fntype);
      delete tsf;
    }
  this->specific_type_functions_are_written_ = true;
}

// Traverse the tree.

void
Gogo::traverse(Traverse* traverse)
{
  // Traverse the current package first for consistency.  The other
  // packages will only contain imported types, constants, and
  // declarations.
  if (this->package_->bindings()->traverse(traverse, true) == TRAVERSE_EXIT)
    return;
  for (Packages::const_iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    {
      if (p->second != this->package_)
	{
	  if (p->second->bindings()->traverse(traverse, true) == TRAVERSE_EXIT)
	    break;
	}
    }
}

// Add a type to verify.  This is used for types of sink variables, in
// order to give appropriate error messages.

void
Gogo::add_type_to_verify(Type* type)
{
  this->verify_types_.push_back(type);
}

// Traversal class used to verify types.

class Verify_types : public Traverse
{
 public:
  Verify_types()
    : Traverse(traverse_types)
  { }

  int
  type(Type*);
};

// Verify that a type is correct.

int
Verify_types::type(Type* t)
{
  if (!t->verify())
    return TRAVERSE_SKIP_COMPONENTS;
  return TRAVERSE_CONTINUE;
}

// Verify that all types are correct.

void
Gogo::verify_types()
{
  Verify_types traverse;
  this->traverse(&traverse);

  for (std::vector<Type*>::iterator p = this->verify_types_.begin();
       p != this->verify_types_.end();
       ++p)
    (*p)->verify();
  this->verify_types_.clear();
}

// Traversal class used to lower parse tree.

class Lower_parse_tree : public Traverse
{
 public:
  Lower_parse_tree(Gogo* gogo, Named_object* function)
    : Traverse(traverse_variables
	       | traverse_constants
	       | traverse_functions
	       | traverse_statements
	       | traverse_expressions),
      gogo_(gogo), function_(function), iota_value_(-1), inserter_()
  { }

  void
  set_inserter(const Statement_inserter* inserter)
  { this->inserter_ = *inserter; }

  int
  variable(Named_object*);

  int
  constant(Named_object*, bool);

  int
  function(Named_object*);

  int
  statement(Block*, size_t* pindex, Statement*);

  int
  expression(Expression**);

 private:
  // General IR.
  Gogo* gogo_;
  // The function we are traversing.
  Named_object* function_;
  // Value to use for the predeclared constant iota.
  int iota_value_;
  // Current statement inserter for use by expressions.
  Statement_inserter inserter_;
};

// Lower variables.

int
Lower_parse_tree::variable(Named_object* no)
{
  if (!no->is_variable())
    return TRAVERSE_CONTINUE;

  if (no->is_variable() && no->var_value()->is_global())
    {
      // Global variables can have loops in their initialization
      // expressions.  This is handled in lower_init_expression.
      no->var_value()->lower_init_expression(this->gogo_, this->function_,
					     &this->inserter_);
      return TRAVERSE_CONTINUE;
    }

  // This is a local variable.  We are going to return
  // TRAVERSE_SKIP_COMPONENTS here because we want to traverse the
  // initialization expression when we reach the variable declaration
  // statement.  However, that means that we need to traverse the type
  // ourselves.
  if (no->var_value()->has_type())
    {
      Type* type = no->var_value()->type();
      if (type != NULL)
	{
	  if (Type::traverse(type, this) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }
  go_assert(!no->var_value()->has_pre_init());

  return TRAVERSE_SKIP_COMPONENTS;
}

// Lower constants.  We handle constants specially so that we can set
// the right value for the predeclared constant iota.  This works in
// conjunction with the way we lower Const_expression objects.

int
Lower_parse_tree::constant(Named_object* no, bool)
{
  Named_constant* nc = no->const_value();

  // Don't get into trouble if the constant's initializer expression
  // refers to the constant itself.
  if (nc->lowering())
    return TRAVERSE_CONTINUE;
  nc->set_lowering();

  go_assert(this->iota_value_ == -1);
  this->iota_value_ = nc->iota_value();
  nc->traverse_expression(this);
  this->iota_value_ = -1;

  nc->clear_lowering();

  // We will traverse the expression a second time, but that will be
  // fast.

  return TRAVERSE_CONTINUE;
}

// Lower the body of a function, and set the closure type.  Record the
// function while lowering it, so that we can pass it down when
// lowering an expression.

int
Lower_parse_tree::function(Named_object* no)
{
  no->func_value()->set_closure_type();

  go_assert(this->function_ == NULL);
  this->function_ = no;
  int t = no->func_value()->traverse(this);
  this->function_ = NULL;

  if (t == TRAVERSE_EXIT)
    return t;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Lower statement parse trees.

int
Lower_parse_tree::statement(Block* block, size_t* pindex, Statement* sorig)
{
  // Because we explicitly traverse the statement's contents
  // ourselves, we want to skip block statements here.  There is
  // nothing to lower in a block statement.
  if (sorig->is_block_statement())
    return TRAVERSE_CONTINUE;

  Statement_inserter hold_inserter(this->inserter_);
  this->inserter_ = Statement_inserter(block, pindex);

  // Lower the expressions first.
  int t = sorig->traverse_contents(this);
  if (t == TRAVERSE_EXIT)
    {
      this->inserter_ = hold_inserter;
      return t;
    }

  // Keep lowering until nothing changes.
  Statement* s = sorig;
  while (true)
    {
      Statement* snew = s->lower(this->gogo_, this->function_, block,
				 &this->inserter_);
      if (snew == s)
	break;
      s = snew;
      t = s->traverse_contents(this);
      if (t == TRAVERSE_EXIT)
	{
	  this->inserter_ = hold_inserter;
	  return t;
	}
    }

  if (s != sorig)
    block->replace_statement(*pindex, s);

  this->inserter_ = hold_inserter;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Lower expression parse trees.

int
Lower_parse_tree::expression(Expression** pexpr)
{
  // We have to lower all subexpressions first, so that we can get
  // their type if necessary.  This is awkward, because we don't have
  // a postorder traversal pass.
  if ((*pexpr)->traverse_subexpressions(this) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  // Keep lowering until nothing changes.
  while (true)
    {
      Expression* e = *pexpr;
      Expression* enew = e->lower(this->gogo_, this->function_,
				  &this->inserter_, this->iota_value_);
      if (enew == e)
	break;
      if (enew->traverse_subexpressions(this) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
      *pexpr = enew;
    }

  // Lower the type of this expression before the parent looks at it,
  // in case the type contains an array that has expressions in its
  // length.  Skip an Unknown_expression, as at this point that means
  // a composite literal key that does not have a type.
  if ((*pexpr)->unknown_expression() == NULL)
    Type::traverse((*pexpr)->type(), this);

  return TRAVERSE_SKIP_COMPONENTS;
}

// Lower the parse tree.  This is called after the parse is complete,
// when all names should be resolved.

void
Gogo::lower_parse_tree()
{
  Lower_parse_tree lower_parse_tree(this, NULL);
  this->traverse(&lower_parse_tree);

  // If we found any functions defined in other packages that are
  // inlinables, import their bodies and turn them into functions.
  //
  // Note that as we import inlinable functions we may find more
  // inlinable functions, so don't use an iterator.
  for (size_t i = 0; i < this->imported_inlinable_functions_.size(); i++)
    {
      Named_object* no = this->imported_inlinable_functions_[i];
      no->func_declaration_value()->import_function_body(this, no);
    }

  // There might be type definitions that involve expressions such as the
  // array length.  Make sure to lower these expressions as well.  Otherwise,
  // errors hidden within a type can introduce unexpected errors into later
  // passes.
  for (std::vector<Type*>::iterator p = this->verify_types_.begin();
       p != this->verify_types_.end();
       ++p)
    Type::traverse(*p, &lower_parse_tree);
}

// Lower a block.

void
Gogo::lower_block(Named_object* function, Block* block)
{
  Lower_parse_tree lower_parse_tree(this, function);
  block->traverse(&lower_parse_tree);
}

// Lower an expression.  INSERTER may be NULL, in which case the
// expression had better not need to create any temporaries.

void
Gogo::lower_expression(Named_object* function, Statement_inserter* inserter,
		       Expression** pexpr)
{
  Lower_parse_tree lower_parse_tree(this, function);
  if (inserter != NULL)
    lower_parse_tree.set_inserter(inserter);
  lower_parse_tree.expression(pexpr);
}

// Lower a constant.  This is called when lowering a reference to a
// constant.  We have to make sure that the constant has already been
// lowered.

void
Gogo::lower_constant(Named_object* no)
{
  go_assert(no->is_const());
  Lower_parse_tree lower(this, NULL);
  lower.constant(no, false);
}

// Make implicit type conversions explicit.  Currently only does for
// interface conversions, so the escape analysis can see them and
// optimize.

class Add_conversions : public Traverse
{
 public:
  Add_conversions()
    : Traverse(traverse_statements
               | traverse_expressions)
  { }

  int
  statement(Block*, size_t* pindex, Statement*);

  int
  expression(Expression**);
};

// Add explicit conversions in a statement.

int
Add_conversions::statement(Block*, size_t*, Statement* sorig)
{
  sorig->add_conversions();
  return TRAVERSE_CONTINUE;
}

// Add explicit conversions in an expression.

int
Add_conversions::expression(Expression** pexpr)
{
  (*pexpr)->add_conversions();
  return TRAVERSE_CONTINUE;
}

void
Gogo::add_conversions()
{
  Add_conversions add_conversions;
  this->traverse(&add_conversions);
}

void
Gogo::add_conversions_in_block(Block *b)
{
  Add_conversions add_conversions;
  b->traverse(&add_conversions);
}

// Traversal class for simple deadcode elimination.

class Remove_deadcode : public Traverse
{
 public:
  Remove_deadcode()
    : Traverse(traverse_statements
               | traverse_expressions)
  { }

  int
  statement(Block*, size_t* pindex, Statement*);

  int
  expression(Expression**);
};

// Remove deadcode in a statement.

int
Remove_deadcode::statement(Block* block, size_t* pindex, Statement* sorig)
{
  Location loc = sorig->location();
  If_statement* ifs = sorig->if_statement();
  if (ifs != NULL)
    {
      // Remove the dead branch of an if statement.
      bool bval;
      if (ifs->condition()->boolean_constant_value(&bval))
        {
          Statement* s;
          if (bval)
            s = Statement::make_block_statement(ifs->then_block(),
                                                loc);
          else
            if (ifs->else_block() != NULL)
              s = Statement::make_block_statement(ifs->else_block(),
                                                  loc);
            else
              // Make a dummy statement.
              s = Statement::make_statement(Expression::make_boolean(false, loc),
                                            true);

          block->replace_statement(*pindex, s);
        }
    }
  return TRAVERSE_CONTINUE;
}

// Remove deadcode in an expression.

int
Remove_deadcode::expression(Expression** pexpr)
{
  // Discard the right arm of a shortcut expression of constant value.
  Binary_expression* be = (*pexpr)->binary_expression();
  bool bval;
  if (be != NULL
      && be->boolean_constant_value(&bval)
      && (be->op() == OPERATOR_ANDAND
          || be->op() == OPERATOR_OROR))
    {
      *pexpr = Expression::make_boolean(bval, be->location());
      Type_context context(NULL, false);
      (*pexpr)->determine_type(&context);
    }
  return TRAVERSE_CONTINUE;
}

// Remove deadcode.

void
Gogo::remove_deadcode()
{
  Remove_deadcode remove_deadcode;
  this->traverse(&remove_deadcode);
}

// Traverse the tree to create function descriptors as needed.

class Create_function_descriptors : public Traverse
{
 public:
  Create_function_descriptors(Gogo* gogo)
    : Traverse(traverse_functions | traverse_expressions),
      gogo_(gogo)
  { }

  int
  function(Named_object*);

  int
  expression(Expression**);

 private:
  Gogo* gogo_;
};

// Create a descriptor for every top-level exported function and every
// function referenced by an inline function.

int
Create_function_descriptors::function(Named_object* no)
{
  if (no->is_function()
      && no->func_value()->enclosing() == NULL
      && !no->func_value()->is_method()
      && ((!Gogo::is_hidden_name(no->name())
	   && !Gogo::is_thunk(no))
	  || no->func_value()->is_referenced_by_inline()))
    no->func_value()->descriptor(this->gogo_, no);

  return TRAVERSE_CONTINUE;
}

// If we see a function referenced in any way other than calling it,
// create a descriptor for it.

int
Create_function_descriptors::expression(Expression** pexpr)
{
  Expression* expr = *pexpr;

  Func_expression* fe = expr->func_expression();
  if (fe != NULL)
    {
      // We would not get here for a call to this function, so this is
      // a reference to a function other than calling it.  We need a
      // descriptor.
      if (fe->closure() != NULL)
	return TRAVERSE_CONTINUE;
      Named_object* no = fe->named_object();
      if (no->is_function() && !no->func_value()->is_method())
	no->func_value()->descriptor(this->gogo_, no);
      else if (no->is_function_declaration()
	       && !no->func_declaration_value()->type()->is_method()
	       && !Linemap::is_predeclared_location(no->location()))
	no->func_declaration_value()->descriptor(this->gogo_, no);
      return TRAVERSE_CONTINUE;
    }

  Bound_method_expression* bme = expr->bound_method_expression();
  if (bme != NULL)
    {
      // We would not get here for a call to this method, so this is a
      // method value.  We need to create a thunk.
      Bound_method_expression::create_thunk(this->gogo_, bme->method(),
					    bme->function());
      return TRAVERSE_CONTINUE;
    }

  Interface_field_reference_expression* ifre =
    expr->interface_field_reference_expression();
  if (ifre != NULL)
    {
      // We would not get here for a call to this interface method, so
      // this is a method value.  We need to create a thunk.
      Interface_type* type = ifre->expr()->type()->interface_type();
      if (type != NULL)
	Interface_field_reference_expression::create_thunk(this->gogo_, type,
							   ifre->name());
      return TRAVERSE_CONTINUE;
    }

  Call_expression* ce = expr->call_expression();
  if (ce != NULL)
    {
      Expression* fn = ce->fn();
      if (fn->func_expression() != NULL
	  || fn->bound_method_expression() != NULL
	  || fn->interface_field_reference_expression() != NULL)
	{
	  // Traverse the arguments but not the function.
	  Expression_list* args = ce->args();
	  if (args != NULL)
	    {
	      if (args->traverse(this) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }

	  // Traverse the subexpressions of the function, if any.
	  if (fn->traverse_subexpressions(this) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;

	  return TRAVERSE_SKIP_COMPONENTS;
	}
    }

  return TRAVERSE_CONTINUE;
}

// Create function descriptors as needed.  We need a function
// descriptor for all exported functions and for all functions that
// are referenced without being called.

void
Gogo::create_function_descriptors()
{
  // Create a function descriptor for any exported function that is
  // declared in this package.  This is so that we have a descriptor
  // for functions written in assembly.  Gather the descriptors first
  // so that we don't add declarations while looping over them.
  std::vector<Named_object*> fndecls;
  Bindings* b = this->package_->bindings();
  for (Bindings::const_declarations_iterator p = b->begin_declarations();
       p != b->end_declarations();
       ++p)
    {
      Named_object* no = p->second;
      if (no->is_function_declaration()
	  && !no->func_declaration_value()->type()->is_method()
	  && !Linemap::is_predeclared_location(no->location())
	  && !Gogo::is_hidden_name(no->name()))
	fndecls.push_back(no);
    }
  for (std::vector<Named_object*>::const_iterator p = fndecls.begin();
       p != fndecls.end();
       ++p)
    (*p)->func_declaration_value()->descriptor(this, *p);
  fndecls.clear();

  Create_function_descriptors cfd(this);
  this->traverse(&cfd);
}

// Finalize the methods of an interface type.

int
Finalize_methods::type(Type* t)
{
  // Check the classification so that we don't finalize the methods
  // twice for a named interface type.
  switch (t->classification())
    {
    case Type::TYPE_INTERFACE:
      t->interface_type()->finalize_methods();
      break;

    case Type::TYPE_NAMED:
      {
	Named_type* nt = t->named_type();

	if (nt->is_alias())
	  return TRAVERSE_CONTINUE;

	Type* rt = nt->real_type();
	if (rt->classification() != Type::TYPE_STRUCT)
	  {
	    // Finalize the methods of the real type first.
	    if (Type::traverse(rt, this) == TRAVERSE_EXIT)
	      return TRAVERSE_EXIT;

	    // Finalize the methods of this type.
	    nt->finalize_methods(this->gogo_);
	  }
	else
	  {
	    // We don't want to finalize the methods of a named struct
	    // type, as the methods should be attached to the named
	    // type, not the struct type.  We just want to finalize
	    // the field types.
	    //
	    // It is possible that a field type refers indirectly to
	    // this type, such as via a field with function type with
	    // an argument or result whose type is this type.  To
	    // avoid the cycle, first finalize the methods of any
	    // embedded types, which are the only types we need to
	    // know to finalize the methods of this type.
	    const Struct_field_list* fields = rt->struct_type()->fields();
	    if (fields != NULL)
	      {
		for (Struct_field_list::const_iterator pf = fields->begin();
		     pf != fields->end();
		     ++pf)
		  {
		    if (pf->is_anonymous())
		      {
			if (Type::traverse(pf->type(), this) == TRAVERSE_EXIT)
			  return TRAVERSE_EXIT;
		      }
		  }
	      }

	    // Finalize the methods of this type.
	    nt->finalize_methods(this->gogo_);

	    // Finalize all the struct fields.
	    if (rt->struct_type()->traverse_field_types(this) == TRAVERSE_EXIT)
	      return TRAVERSE_EXIT;
	  }

	// If this type is defined in a different package, then finalize the
	// types of all the methods, since we won't see them otherwise.
	if (nt->named_object()->package() != NULL && nt->has_any_methods())
	  {
	    const Methods* methods = nt->methods();
	    for (Methods::const_iterator p = methods->begin();
		 p != methods->end();
		 ++p)
	      {
		if (Type::traverse(p->second->type(), this) == TRAVERSE_EXIT)
		  return TRAVERSE_EXIT;
	      }
	  }

	// Finalize the types of all methods that are declared but not
	// defined, since we won't see the declarations otherwise.
	if (nt->named_object()->package() == NULL
	    && nt->local_methods() != NULL)
	  {
	    const Bindings* methods = nt->local_methods();
	    for (Bindings::const_declarations_iterator p =
		   methods->begin_declarations();
		 p != methods->end_declarations();
		 p++)
	      {
		if (p->second->is_function_declaration())
		  {
		    Type* mt = p->second->func_declaration_value()->type();
		    if (Type::traverse(mt, this) == TRAVERSE_EXIT)
		      return TRAVERSE_EXIT;
		  }
	      }
	  }

	return TRAVERSE_SKIP_COMPONENTS;
      }

    case Type::TYPE_STRUCT:
      // Traverse the field types first in case there is an embedded
      // field with methods that the struct should inherit.
      if (t->struct_type()->traverse_field_types(this) == TRAVERSE_EXIT)
          return TRAVERSE_EXIT;
      t->struct_type()->finalize_methods(this->gogo_);
      return TRAVERSE_SKIP_COMPONENTS;

    default:
      break;
    }

  return TRAVERSE_CONTINUE;
}

// Finalize method lists and build stub methods for types.

void
Gogo::finalize_methods()
{
  Finalize_methods finalize(this);
  this->traverse(&finalize);
}

// Finalize the method list for a type.  This is called when a type is
// parsed for an inlined function body, which happens after the
// finalize_methods pass.

void
Gogo::finalize_methods_for_type(Type* type)
{
  Finalize_methods finalize(this);
  Type::traverse(type, &finalize);
}

// Set types for unspecified variables and constants.

void
Gogo::determine_types()
{
  Bindings* bindings = this->current_bindings();
  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p)
    {
      if ((*p)->is_function())
	(*p)->func_value()->determine_types();
      else if ((*p)->is_variable())
	(*p)->var_value()->determine_type();
      else if ((*p)->is_const())
	(*p)->const_value()->determine_type();

      // See if a variable requires us to build an initialization
      // function.  We know that we will see all global variables
      // here.
      if (!this->need_init_fn_ && (*p)->is_variable())
	{
	  Variable* variable = (*p)->var_value();

	  // If this is a global variable which requires runtime
	  // initialization, we need an initialization function.
	  if (!variable->is_global())
	    ;
	  else if (variable->init() == NULL)
	    ;
	  else if (variable->type()->interface_type() != NULL)
	    this->need_init_fn_ = true;
	  else if (variable->init()->is_constant())
	    ;
	  else if (!variable->init()->is_composite_literal())
	    this->need_init_fn_ = true;
	  else if (variable->init()->is_nonconstant_composite_literal())
	    this->need_init_fn_ = true;

	  // If this is a global variable which holds a pointer value,
	  // then we need an initialization function to register it as a
	  // GC root.
	  if (variable->is_global() && variable->type()->has_pointer())
	    this->need_init_fn_ = true;
	}
    }

  // Determine the types of constants in packages.
  for (Packages::const_iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    p->second->determine_types();
}

// Traversal class used for type checking.

class Check_types_traverse : public Traverse
{
 public:
  Check_types_traverse(Gogo* gogo)
    : Traverse(traverse_variables
	       | traverse_constants
	       | traverse_functions
	       | traverse_statements
	       | traverse_expressions),
      gogo_(gogo)
  { }

  int
  variable(Named_object*);

  int
  constant(Named_object*, bool);

  int
  function(Named_object*);

  int
  statement(Block*, size_t* pindex, Statement*);

  int
  expression(Expression**);

 private:
  // General IR.
  Gogo* gogo_;
};

// Check that a variable initializer has the right type.

int
Check_types_traverse::variable(Named_object* named_object)
{
  if (named_object->is_variable())
    {
      Variable* var = named_object->var_value();

      // Give error if variable type is not defined.
      var->type()->base();

      Expression* init = var->init();
      std::string reason;
      if (init != NULL
	  && !Type::are_assignable(var->type(), init->type(), &reason))
	{
	  if (reason.empty())
	    go_error_at(var->location(), "incompatible type in initialization");
	  else
	    go_error_at(var->location(),
			"incompatible type in initialization (%s)",
			reason.c_str());
          init = Expression::make_error(named_object->location());
	  var->clear_init();
	}
      else if (init != NULL
               && init->func_expression() != NULL)
        {
          Named_object* no = init->func_expression()->named_object();
          Function_type* fntype;
          if (no->is_function())
            fntype = no->func_value()->type();
          else if (no->is_function_declaration())
            fntype = no->func_declaration_value()->type();
          else
            go_unreachable();

          // Builtin functions cannot be used as function values for variable
          // initialization.
          if (fntype->is_builtin())
            {
	      go_error_at(init->location(),
			  "invalid use of special built-in function %qs; "
			  "must be called",
			  no->message_name().c_str());
            }
        }
      if (!var->is_used()
          && !var->is_global()
          && !var->is_parameter()
          && !var->is_receiver()
          && !var->type()->is_error()
          && (init == NULL || !init->is_error_expression())
          && !Lex::is_invalid_identifier(named_object->name()))
	go_error_at(var->location(), "%qs declared but not used",
		    named_object->message_name().c_str());
    }
  return TRAVERSE_CONTINUE;
}

// Check that a constant initializer has the right type.

int
Check_types_traverse::constant(Named_object* named_object, bool)
{
  Named_constant* constant = named_object->const_value();
  Type* ctype = constant->type();
  if (ctype->integer_type() == NULL
      && ctype->float_type() == NULL
      && ctype->complex_type() == NULL
      && !ctype->is_boolean_type()
      && !ctype->is_string_type())
    {
      if (ctype->is_nil_type())
	go_error_at(constant->location(), "const initializer cannot be nil");
      else if (!ctype->is_error())
	go_error_at(constant->location(), "invalid constant type");
      constant->set_error();
    }
  else if (!constant->expr()->is_constant())
    {
      go_error_at(constant->expr()->location(), "expression is not constant");
      constant->set_error();
    }
  else if (!Type::are_assignable(constant->type(), constant->expr()->type(),
				 NULL))
    {
      go_error_at(constant->location(),
                  "initialization expression has wrong type");
      constant->set_error();
    }
  return TRAVERSE_CONTINUE;
}

// There are no types to check in a function, but this is where we
// issue warnings about labels which are defined but not referenced.

int
Check_types_traverse::function(Named_object* no)
{
  no->func_value()->check_labels();
  return TRAVERSE_CONTINUE;
}

// Check that types are valid in a statement.

int
Check_types_traverse::statement(Block*, size_t*, Statement* s)
{
  s->check_types(this->gogo_);
  return TRAVERSE_CONTINUE;
}

// Check that types are valid in an expression.

int
Check_types_traverse::expression(Expression** expr)
{
  (*expr)->check_types(this->gogo_);
  return TRAVERSE_CONTINUE;
}

// Check that types are valid.

void
Gogo::check_types()
{
  Check_types_traverse traverse(this);
  this->traverse(&traverse);

  Bindings* bindings = this->current_bindings();
  for (Bindings::const_declarations_iterator p = bindings->begin_declarations();
       p != bindings->end_declarations();
       ++p)
    {
      // Also check the types in a function declaration's signature.
      Named_object* no = p->second;
      if (no->is_function_declaration())
        no->func_declaration_value()->check_types();
    }
}

// Check the types in a single block.

void
Gogo::check_types_in_block(Block* block)
{
  Check_types_traverse traverse(this);
  block->traverse(&traverse);
}

// For each global variable defined in the current package, record the
// set of variables that its initializer depends on.  We do this after
// lowering so that all unknown names are resolved to their final
// locations.  We do this before write barrier insertion because that
// makes it harder to distinguish references from assignments in
// preinit blocks.

void
Gogo::record_global_init_refs()
{
  Bindings* bindings = this->package_->bindings();
  for (Bindings::const_definitions_iterator pb = bindings->begin_definitions();
       pb != bindings->end_definitions();
       pb++)
    {
      Named_object* no = *pb;
      if (!no->is_variable())
	continue;

      Variable* var = no->var_value();
      go_assert(var->is_global());

      Find_vars find_vars;
      Expression* init = var->init();
      if (init != NULL)
	Expression::traverse(&init, &find_vars);
      if (var->has_pre_init())
	var->preinit()->traverse(&find_vars);
      Named_object* dep = this->var_depends_on(var);
      if (dep != NULL)
	{
	  Expression* dinit = dep->var_value()->init();
	  if (dinit != NULL)
	    Expression::traverse(&dinit, &find_vars);
	  if (dep->var_value()->has_pre_init())
	    dep->var_value()->preinit()->traverse(&find_vars);
	}

      for (Find_vars::const_iterator pv = find_vars.begin();
	   pv != find_vars.end();
	   ++pv)
	var->add_init_ref(*pv);
    }
}

// A traversal class which finds all the expressions which must be
// evaluated in order within a statement or larger expression.  This
// is used to implement the rules about order of evaluation.

class Find_eval_ordering : public Traverse
{
 private:
  typedef std::vector<Expression**> Expression_pointers;

 public:
  Find_eval_ordering()
    : Traverse(traverse_blocks
	       | traverse_statements
	       | traverse_expressions),
      exprs_()
  { }

  size_t
  size() const
  { return this->exprs_.size(); }

  typedef Expression_pointers::const_iterator const_iterator;

  const_iterator
  begin() const
  { return this->exprs_.begin(); }

  const_iterator
  end() const
  { return this->exprs_.end(); }

 protected:
  int
  block(Block*)
  { return TRAVERSE_SKIP_COMPONENTS; }

  int
  statement(Block*, size_t*, Statement*)
  { return TRAVERSE_SKIP_COMPONENTS; }

  int
  expression(Expression**);

 private:
  // A list of pointers to expressions with side-effects.
  Expression_pointers exprs_;
};

// If an expression must be evaluated in order, put it on the list.

int
Find_eval_ordering::expression(Expression** expression_pointer)
{
  Binary_expression* binexp = (*expression_pointer)->binary_expression();
  if (binexp != NULL
      && (binexp->op() == OPERATOR_ANDAND || binexp->op() == OPERATOR_OROR))
    {
      // Shortcut expressions may potentially have side effects which need
      // to be ordered, so add them to the list.
      // We don't order its subexpressions here since they may be evaluated
      // conditionally. This is handled in remove_shortcuts.
      this->exprs_.push_back(expression_pointer);
      return TRAVERSE_SKIP_COMPONENTS;
    }

  // We have to look at subexpressions before this one.
  if ((*expression_pointer)->traverse_subexpressions(this) == TRAVERSE_EXIT)
    return TRAVERSE_EXIT;
  if ((*expression_pointer)->must_eval_in_order())
    this->exprs_.push_back(expression_pointer);
  return TRAVERSE_SKIP_COMPONENTS;
}

// A traversal class for ordering evaluations.

class Order_eval : public Traverse
{
 public:
  Order_eval(Gogo* gogo)
    : Traverse(traverse_variables
	       | traverse_statements),
      gogo_(gogo)
  { }

  int
  variable(Named_object*);

  int
  statement(Block*, size_t*, Statement*);

 private:
  // The IR.
  Gogo* gogo_;
};

// Implement the order of evaluation rules for a statement.

int
Order_eval::statement(Block* block, size_t* pindex, Statement* stmt)
{
  // FIXME: This approach doesn't work for switch statements, because
  // we add the new statements before the whole switch when we need to
  // instead add them just before the switch expression.  The right
  // fix is probably to lower switch statements with nonconstant cases
  // to a series of conditionals.
  if (stmt->switch_statement() != NULL)
    return TRAVERSE_CONTINUE;

  Find_eval_ordering find_eval_ordering;

  // If S is a variable declaration, then ordinary traversal won't do
  // anything.  We want to explicitly traverse the initialization
  // expression if there is one.
  Variable_declaration_statement* vds = stmt->variable_declaration_statement();
  Expression* init = NULL;
  Expression* orig_init = NULL;
  if (vds == NULL)
    stmt->traverse_contents(&find_eval_ordering);
  else
    {
      init = vds->var()->var_value()->init();
      if (init == NULL)
	return TRAVERSE_CONTINUE;
      orig_init = init;

      // It might seem that this could be
      // init->traverse_subexpressions.  Unfortunately that can fail
      // in a case like
      //   var err os.Error
      //   newvar, err := call(arg())
      // Here newvar will have an init of call result 0 of
      // call(arg()).  If we only traverse subexpressions, we will
      // only find arg(), and we won't bother to move anything out.
      // Then we get to the assignment to err, we will traverse the
      // whole statement, and this time we will find both call() and
      // arg(), and so we will move them out.  This will cause them to
      // be put into temporary variables before the assignment to err
      // but after the declaration of newvar.  To avoid that problem,
      // we traverse the entire expression here.
      Expression::traverse(&init, &find_eval_ordering);
    }

  size_t c = find_eval_ordering.size();
  if (c == 0)
    return TRAVERSE_CONTINUE;

  // If there is only one expression with a side-effect, we can
  // usually leave it in place.
  if (c == 1)
    {
      switch (stmt->classification())
	{
	case Statement::STATEMENT_ASSIGNMENT:
	  // For an assignment statement, we need to evaluate an
	  // expression on the right hand side before we evaluate any
	  // index expression on the left hand side, so for that case
	  // we always move the expression.  Otherwise we mishandle
	  // m[0] = len(m) where m is a map.
	  break;

	case Statement::STATEMENT_EXPRESSION:
	  {
	    // If this is a call statement that doesn't return any
	    // values, it will not have been counted as a value to
	    // move.  We need to move any subexpressions in case they
	    // are themselves call statements that require passing a
	    // closure.
	    Expression* expr = stmt->expression_statement()->expr();
	    if (expr->call_expression() != NULL
		&& expr->call_expression()->result_count() == 0)
	      break;
	    return TRAVERSE_CONTINUE;
	  }

	default:
	  // We can leave the expression in place.
	  return TRAVERSE_CONTINUE;
	}
    }

  bool is_thunk = stmt->thunk_statement() != NULL;
  Expression_statement* es = stmt->expression_statement();
  for (Find_eval_ordering::const_iterator p = find_eval_ordering.begin();
       p != find_eval_ordering.end();
       ++p)
    {
      Expression** pexpr = *p;

      // The last expression in a thunk will be the call passed to go
      // or defer, which we must not evaluate early.
      if (is_thunk && p + 1 == find_eval_ordering.end())
	break;

      Location loc = (*pexpr)->location();
      Statement* s;
      if ((*pexpr)->call_expression() == NULL
	  || (*pexpr)->call_expression()->result_count() < 2)
	{
	  Temporary_statement* ts = Statement::make_temporary(NULL, *pexpr,
							      loc);
	  s = ts;
	  *pexpr = Expression::make_temporary_reference(ts, loc);
	}
      else
	{
	  // A call expression which returns multiple results needs to
	  // be handled specially.  We can't create a temporary
	  // because there is no type to give it.  Any actual uses of
	  // the values will be done via Call_result_expressions.
          //
          // Since a given call expression can be shared by multiple
          // Call_result_expressions, avoid hoisting the call the
          // second time we see it here. In addition, don't try to
          // hoist the top-level multi-return call in the statement,
          // since doing this would result a tree with more than one copy
          // of the call.
          if (this->remember_expression(*pexpr))
            s = NULL;
          else if (es != NULL && *pexpr == es->expr())
            s = NULL;
          else
            s = Statement::make_statement(*pexpr, true);
        }

      if (s != NULL)
        {
          block->insert_statement_before(*pindex, s);
          ++*pindex;
        }
    }

  if (init != orig_init)
    vds->var()->var_value()->set_init(init);

  return TRAVERSE_CONTINUE;
}

// Implement the order of evaluation rules for the initializer of a
// global variable.

int
Order_eval::variable(Named_object* no)
{
  if (no->is_result_variable())
    return TRAVERSE_CONTINUE;
  Variable* var = no->var_value();
  Expression* init = var->init();
  if (!var->is_global() || init == NULL)
    return TRAVERSE_CONTINUE;

  Find_eval_ordering find_eval_ordering;
  Expression::traverse(&init, &find_eval_ordering);

  if (find_eval_ordering.size() <= 1)
    {
      // If there is only one expression with a side-effect, we can
      // leave it in place.
      return TRAVERSE_SKIP_COMPONENTS;
    }

  Expression* orig_init = init;

  for (Find_eval_ordering::const_iterator p = find_eval_ordering.begin();
       p != find_eval_ordering.end();
       ++p)
    {
      Expression** pexpr = *p;
      Location loc = (*pexpr)->location();
      Statement* s;
      if ((*pexpr)->call_expression() == NULL
	  || (*pexpr)->call_expression()->result_count() < 2)
	{
	  Temporary_statement* ts = Statement::make_temporary(NULL, *pexpr,
							      loc);
	  s = ts;
	  *pexpr = Expression::make_temporary_reference(ts, loc);
	}
      else
	{
	  // A call expression which returns multiple results needs to
	  // be handled specially.
	  s = Statement::make_statement(*pexpr, true);
	}
      var->add_preinit_statement(this->gogo_, s);
    }

  if (init != orig_init)
    var->set_init(init);

  return TRAVERSE_SKIP_COMPONENTS;
}

// Use temporary variables to implement the order of evaluation rules.

void
Gogo::order_evaluations()
{
  Order_eval order_eval(this);
  this->traverse(&order_eval);
}

// Order evaluations in a block.

void
Gogo::order_block(Block* block)
{
  Order_eval order_eval(this);
  block->traverse(&order_eval);
}

// A traversal class used to find a single shortcut operator within an
// expression.

class Find_shortcut : public Traverse
{
 public:
  Find_shortcut()
    : Traverse(traverse_blocks
	       | traverse_statements
	       | traverse_expressions),
      found_(NULL)
  { }

  // A pointer to the expression which was found, or NULL if none was
  // found.
  Expression**
  found() const
  { return this->found_; }

 protected:
  int
  block(Block*)
  { return TRAVERSE_SKIP_COMPONENTS; }

  int
  statement(Block*, size_t*, Statement*)
  { return TRAVERSE_SKIP_COMPONENTS; }

  int
  expression(Expression**);

 private:
  Expression** found_;
};

// Find a shortcut expression.

int
Find_shortcut::expression(Expression** pexpr)
{
  Expression* expr = *pexpr;
  Binary_expression* be = expr->binary_expression();
  if (be == NULL)
    return TRAVERSE_CONTINUE;
  Operator op = be->op();
  if (op != OPERATOR_OROR && op != OPERATOR_ANDAND)
    return TRAVERSE_CONTINUE;
  go_assert(this->found_ == NULL);
  this->found_ = pexpr;
  return TRAVERSE_EXIT;
}

// A traversal class used to turn shortcut operators into explicit if
// statements.

class Shortcuts : public Traverse
{
 public:
  Shortcuts(Gogo* gogo)
    : Traverse(traverse_variables
	       | traverse_statements),
      gogo_(gogo)
  { }

 protected:
  int
  variable(Named_object*);

  int
  statement(Block*, size_t*, Statement*);

 private:
  // Convert a shortcut operator.
  Statement*
  convert_shortcut(Block* enclosing, Expression** pshortcut);

  // The IR.
  Gogo* gogo_;
};

// Remove shortcut operators in a single statement.

int
Shortcuts::statement(Block* block, size_t* pindex, Statement* s)
{
  // FIXME: This approach doesn't work for switch statements, because
  // we add the new statements before the whole switch when we need to
  // instead add them just before the switch expression.  The right
  // fix is probably to lower switch statements with nonconstant cases
  // to a series of conditionals.
  if (s->switch_statement() != NULL)
    return TRAVERSE_CONTINUE;

  while (true)
    {
      Find_shortcut find_shortcut;

      // If S is a variable declaration, then ordinary traversal won't
      // do anything.  We want to explicitly traverse the
      // initialization expression if there is one.
      Variable_declaration_statement* vds = s->variable_declaration_statement();
      Expression* init = NULL;
      if (vds == NULL)
	s->traverse_contents(&find_shortcut);
      else
	{
	  init = vds->var()->var_value()->init();
	  if (init == NULL)
	    return TRAVERSE_CONTINUE;
	  init->traverse(&init, &find_shortcut);
	}
      Expression** pshortcut = find_shortcut.found();
      if (pshortcut == NULL)
	return TRAVERSE_CONTINUE;

      Statement* snew = this->convert_shortcut(block, pshortcut);
      block->insert_statement_before(*pindex, snew);
      ++*pindex;

      if (pshortcut == &init)
	vds->var()->var_value()->set_init(init);
    }
}

// Remove shortcut operators in the initializer of a global variable.

int
Shortcuts::variable(Named_object* no)
{
  if (no->is_result_variable())
    return TRAVERSE_CONTINUE;
  Variable* var = no->var_value();
  Expression* init = var->init();
  if (!var->is_global() || init == NULL)
    return TRAVERSE_CONTINUE;

  while (true)
    {
      Find_shortcut find_shortcut;
      init->traverse(&init, &find_shortcut);
      Expression** pshortcut = find_shortcut.found();
      if (pshortcut == NULL)
	return TRAVERSE_CONTINUE;

      Statement* snew = this->convert_shortcut(NULL, pshortcut);
      var->add_preinit_statement(this->gogo_, snew);
      if (pshortcut == &init)
	var->set_init(init);
    }
}

// Given an expression which uses a shortcut operator, return a
// statement which implements it, and update *PSHORTCUT accordingly.

Statement*
Shortcuts::convert_shortcut(Block* enclosing, Expression** pshortcut)
{
  Binary_expression* shortcut = (*pshortcut)->binary_expression();
  Expression* left = shortcut->left();
  Expression* right = shortcut->right();
  Location loc = shortcut->location();

  Block* retblock = new Block(enclosing, loc);
  retblock->set_end_location(loc);

  Temporary_statement* ts = Statement::make_temporary(shortcut->type(),
						      left, loc);
  retblock->add_statement(ts);

  Block* block = new Block(retblock, loc);
  block->set_end_location(loc);
  Expression* tmpref = Expression::make_temporary_reference(ts, loc);
  Statement* assign = Statement::make_assignment(tmpref, right, loc);
  block->add_statement(assign);

  Expression* cond = Expression::make_temporary_reference(ts, loc);
  if (shortcut->binary_expression()->op() == OPERATOR_OROR)
    cond = Expression::make_unary(OPERATOR_NOT, cond, loc);

  Statement* if_statement = Statement::make_if_statement(cond, block, NULL,
							 loc);
  retblock->add_statement(if_statement);

  *pshortcut = Expression::make_temporary_reference(ts, loc);

  delete shortcut;

  // Now convert any shortcut operators in LEFT and RIGHT.
  // LEFT and RIGHT were skipped in the top level
  // Gogo::order_evaluations. We need to order their
  // components first.
  Order_eval order_eval(this->gogo_);
  retblock->traverse(&order_eval);
  Shortcuts shortcuts(this->gogo_);
  retblock->traverse(&shortcuts);

  return Statement::make_block_statement(retblock, loc);
}

// Turn shortcut operators into explicit if statements.  Doing this
// considerably simplifies the order of evaluation rules.

void
Gogo::remove_shortcuts()
{
  Shortcuts shortcuts(this);
  this->traverse(&shortcuts);
}

// Turn shortcut operators into explicit if statements in a block.

void
Gogo::remove_shortcuts_in_block(Block* block)
{
  Shortcuts shortcuts(this);
  block->traverse(&shortcuts);
}

// Traversal to flatten parse tree after order of evaluation rules are applied.

class Flatten : public Traverse
{
 public:
  Flatten(Gogo* gogo, Named_object* function)
    : Traverse(traverse_variables
	       | traverse_functions
	       | traverse_statements
	       | traverse_expressions),
      gogo_(gogo), function_(function), inserter_()
  { }

  void
  set_inserter(const Statement_inserter* inserter)
  { this->inserter_ = *inserter; }

  int
  variable(Named_object*);

  int
  function(Named_object*);

  int
  statement(Block*, size_t* pindex, Statement*);

  int
  expression(Expression**);

 private:
  // General IR.
  Gogo* gogo_;
  // The function we are traversing.
  Named_object* function_;
  // Current statement inserter for use by expressions.
  Statement_inserter inserter_;
};

// Flatten variables.

int
Flatten::variable(Named_object* no)
{
  if (!no->is_variable())
    return TRAVERSE_CONTINUE;

  if (no->is_variable() && no->var_value()->is_global())
    {
      // Global variables can have loops in their initialization
      // expressions.  This is handled in flatten_init_expression.
      no->var_value()->flatten_init_expression(this->gogo_, this->function_,
                                               &this->inserter_);
      return TRAVERSE_CONTINUE;
    }

  if (!no->var_value()->is_parameter()
      && !no->var_value()->is_receiver()
      && !no->var_value()->is_closure()
      && no->var_value()->is_non_escaping_address_taken()
      && !no->var_value()->is_in_heap()
      && no->var_value()->toplevel_decl() == NULL)
    {
      // Local variable that has address taken but not escape.
      // It needs to be live beyond its lexical scope. So we
      // create a top-level declaration for it.
      // No need to do it if it is already in the top level.
      Block* top_block = function_->func_value()->block();
      if (top_block->bindings()->lookup_local(no->name()) != no)
        {
          Variable* var = no->var_value();
          Temporary_statement* ts =
            Statement::make_temporary(var->type(), NULL, var->location());
          ts->set_is_address_taken();
          top_block->add_statement_at_front(ts);
          var->set_toplevel_decl(ts);
        }
    }

  go_assert(!no->var_value()->has_pre_init());

  return TRAVERSE_SKIP_COMPONENTS;
}

// Flatten the body of a function.  Record the function while flattening it,
// so that we can pass it down when flattening an expression.

int
Flatten::function(Named_object* no)
{
  go_assert(this->function_ == NULL);
  this->function_ = no;
  int t = no->func_value()->traverse(this);
  this->function_ = NULL;

  if (t == TRAVERSE_EXIT)
    return t;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Flatten statement parse trees.

int
Flatten::statement(Block* block, size_t* pindex, Statement* sorig)
{
  // Because we explicitly traverse the statement's contents
  // ourselves, we want to skip block statements here.  There is
  // nothing to flatten in a block statement.
  if (sorig->is_block_statement())
    return TRAVERSE_CONTINUE;

  Statement_inserter hold_inserter(this->inserter_);
  this->inserter_ = Statement_inserter(block, pindex);

  // Flatten the expressions first.
  int t = sorig->traverse_contents(this);
  if (t == TRAVERSE_EXIT)
    {
      this->inserter_ = hold_inserter;
      return t;
    }

  // Keep flattening until nothing changes.
  Statement* s = sorig;
  while (true)
    {
      Statement* snew = s->flatten(this->gogo_, this->function_, block,
                                   &this->inserter_);
      if (snew == s)
	break;
      s = snew;
      t = s->traverse_contents(this);
      if (t == TRAVERSE_EXIT)
	{
	  this->inserter_ = hold_inserter;
	  return t;
	}
    }

  if (s != sorig)
    block->replace_statement(*pindex, s);

  this->inserter_ = hold_inserter;
  return TRAVERSE_SKIP_COMPONENTS;
}

// Flatten expression parse trees.

int
Flatten::expression(Expression** pexpr)
{
  // Keep flattening until nothing changes.
  while (true)
    {
      Expression* e = *pexpr;
      if (e->traverse_subexpressions(this) == TRAVERSE_EXIT)
        return TRAVERSE_EXIT;

      Expression* enew = e->flatten(this->gogo_, this->function_,
                                    &this->inserter_);
      if (enew == e)
	break;
      *pexpr = enew;
    }
  return TRAVERSE_SKIP_COMPONENTS;
}

// Flatten a block.

void
Gogo::flatten_block(Named_object* function, Block* block)
{
  Flatten flatten(this, function);
  block->traverse(&flatten);
}

// Flatten an expression.  INSERTER may be NULL, in which case the
// expression had better not need to create any temporaries.

void
Gogo::flatten_expression(Named_object* function, Statement_inserter* inserter,
                         Expression** pexpr)
{
  Flatten flatten(this, function);
  if (inserter != NULL)
    flatten.set_inserter(inserter);
  flatten.expression(pexpr);
}

void
Gogo::flatten()
{
  Flatten flatten(this, NULL);
  this->traverse(&flatten);
}

// Traversal to convert calls to the predeclared recover function to
// pass in an argument indicating whether it can recover from a panic
// or not.

class Convert_recover : public Traverse
{
 public:
  Convert_recover(Named_object* arg)
    : Traverse(traverse_expressions),
      arg_(arg)
  { }

 protected:
  int
  expression(Expression**);

 private:
  // The argument to pass to the function.
  Named_object* arg_;
};

// Convert calls to recover.

int
Convert_recover::expression(Expression** pp)
{
  Call_expression* ce = (*pp)->call_expression();
  if (ce != NULL && ce->is_recover_call())
    ce->set_recover_arg(Expression::make_var_reference(this->arg_,
						       ce->location()));
  return TRAVERSE_CONTINUE;
}

// Traversal for build_recover_thunks.

class Build_recover_thunks : public Traverse
{
 public:
  Build_recover_thunks(Gogo* gogo)
    : Traverse(traverse_functions),
      gogo_(gogo)
  { }

  int
  function(Named_object*);

 private:
  Expression*
  can_recover_arg(Location);

  // General IR.
  Gogo* gogo_;
};

// If this function calls recover, turn it into a thunk.

int
Build_recover_thunks::function(Named_object* orig_no)
{
  Function* orig_func = orig_no->func_value();
  if (!orig_func->calls_recover()
      || orig_func->is_recover_thunk()
      || orig_func->has_recover_thunk())
    return TRAVERSE_CONTINUE;

  Gogo* gogo = this->gogo_;
  Location location = orig_func->location();

  static int count;
  char buf[50];

  Function_type* orig_fntype = orig_func->type();
  Typed_identifier_list* new_params = new Typed_identifier_list();
  std::string receiver_name;
  if (orig_fntype->is_method())
    {
      const Typed_identifier* receiver = orig_fntype->receiver();
      snprintf(buf, sizeof buf, "rt.%u", count);
      ++count;
      receiver_name = buf;
      new_params->push_back(Typed_identifier(receiver_name, receiver->type(),
					     receiver->location()));
    }
  const Typed_identifier_list* orig_params = orig_fntype->parameters();
  if (orig_params != NULL && !orig_params->empty())
    {
      for (Typed_identifier_list::const_iterator p = orig_params->begin();
	   p != orig_params->end();
	   ++p)
	{
	  snprintf(buf, sizeof buf, "pt.%u", count);
	  ++count;
	  new_params->push_back(Typed_identifier(buf, p->type(),
						 p->location()));
	}
    }
  snprintf(buf, sizeof buf, "pr.%u", count);
  ++count;
  std::string can_recover_name = buf;
  new_params->push_back(Typed_identifier(can_recover_name,
					 Type::lookup_bool_type(),
					 orig_fntype->location()));

  const Typed_identifier_list* orig_results = orig_fntype->results();
  Typed_identifier_list* new_results;
  if (orig_results == NULL || orig_results->empty())
    new_results = NULL;
  else
    {
      new_results = new Typed_identifier_list();
      for (Typed_identifier_list::const_iterator p = orig_results->begin();
	   p != orig_results->end();
	   ++p)
	new_results->push_back(Typed_identifier("", p->type(), p->location()));
    }

  Function_type *new_fntype = Type::make_function_type(NULL, new_params,
						       new_results,
						       orig_fntype->location());
  if (orig_fntype->is_varargs())
    new_fntype->set_is_varargs();

  Type* rtype = NULL;
  if (orig_fntype->is_method())
    rtype = orig_fntype->receiver()->type();
  std::string name(gogo->recover_thunk_name(orig_no->name(), rtype));
  Named_object *new_no = gogo->start_function(name, new_fntype, false,
					      location);
  Function *new_func = new_no->func_value();
  if (orig_func->enclosing() != NULL)
    new_func->set_enclosing(orig_func->enclosing());

  // We build the code for the original function attached to the new
  // function, and then swap the original and new function bodies.
  // This means that existing references to the original function will
  // then refer to the new function.  That makes this code a little
  // confusing, in that the reference to NEW_NO really refers to the
  // other function, not the one we are building.

  Expression* closure = NULL;
  if (orig_func->needs_closure())
    {
      // For the new function we are creating, declare a new parameter
      // variable NEW_CLOSURE_NO and set it to be the closure variable
      // of the function.  This will be set to the closure value
      // passed in by the caller.  Then pass a reference to this
      // variable as the closure value when calling the original
      // function.  In other words, simply pass the closure value
      // through the thunk we are creating.
      Named_object* orig_closure_no = orig_func->closure_var();
      Variable* orig_closure_var = orig_closure_no->var_value();
      Variable* new_var = new Variable(orig_closure_var->type(), NULL, false,
				       false, false, location);
      new_var->set_is_closure();
      snprintf(buf, sizeof buf, "closure.%u", count);
      ++count;
      Named_object* new_closure_no = Named_object::make_variable(buf, NULL,
								 new_var);
      new_func->set_closure_var(new_closure_no);
      closure = Expression::make_var_reference(new_closure_no, location);
    }

  Expression* fn = Expression::make_func_reference(new_no, closure, location);

  Expression_list* args = new Expression_list();
  if (new_params != NULL)
    {
      // Note that we skip the last parameter, which is the boolean
      // indicating whether recover can succed.
      for (Typed_identifier_list::const_iterator p = new_params->begin();
	   p + 1 != new_params->end();
	   ++p)
	{
	  Named_object* p_no = gogo->lookup(p->name(), NULL);
	  go_assert(p_no != NULL
		     && p_no->is_variable()
		     && p_no->var_value()->is_parameter());
	  args->push_back(Expression::make_var_reference(p_no, location));
	}
    }
  args->push_back(this->can_recover_arg(location));

  gogo->start_block(location);

  Call_expression* call = Expression::make_call(fn, args, false, location);

  // Any varargs call has already been lowered.
  call->set_varargs_are_lowered();

  Statement* s = Statement::make_return_from_call(call, location);
  s->determine_types();
  gogo->add_statement(s);

  Block* b = gogo->finish_block(location);

  gogo->add_block(b, location);

  // Lower the call in case it returns multiple results.
  gogo->lower_block(new_no, b);

  gogo->finish_function(location);

  // Swap the function bodies and types.
  new_func->swap_for_recover(orig_func);
  orig_func->set_is_recover_thunk();
  new_func->set_calls_recover();
  new_func->set_has_recover_thunk();

  Bindings* orig_bindings = orig_func->block()->bindings();
  Bindings* new_bindings = new_func->block()->bindings();
  if (orig_fntype->is_method())
    {
      // We changed the receiver to be a regular parameter.  We have
      // to update the binding accordingly in both functions.
      Named_object* orig_rec_no = orig_bindings->lookup_local(receiver_name);
      go_assert(orig_rec_no != NULL
		 && orig_rec_no->is_variable()
		 && !orig_rec_no->var_value()->is_receiver());
      orig_rec_no->var_value()->set_is_receiver();

      std::string new_receiver_name(orig_fntype->receiver()->name());
      if (new_receiver_name.empty())
	{
	  // Find the receiver.  It was named "r.NNN" in
	  // Gogo::start_function.
	  for (Bindings::const_definitions_iterator p =
		 new_bindings->begin_definitions();
	       p != new_bindings->end_definitions();
	       ++p)
	    {
	      const std::string& pname((*p)->name());
	      if (pname[0] == 'r' && pname[1] == '.')
		{
		  new_receiver_name = pname;
		  break;
		}
	    }
	  go_assert(!new_receiver_name.empty());
	}
      Named_object* new_rec_no = new_bindings->lookup_local(new_receiver_name);
      if (new_rec_no == NULL)
	go_assert(saw_errors());
      else
	{
	  go_assert(new_rec_no->is_variable()
		     && new_rec_no->var_value()->is_receiver());
	  new_rec_no->var_value()->set_is_not_receiver();
	}
    }

  // Because we flipped blocks but not types, the can_recover
  // parameter appears in the (now) old bindings as a parameter.
  // Change it to a local variable, whereupon it will be discarded.
  Named_object* can_recover_no = orig_bindings->lookup_local(can_recover_name);
  go_assert(can_recover_no != NULL
	     && can_recover_no->is_variable()
	     && can_recover_no->var_value()->is_parameter());
  orig_bindings->remove_binding(can_recover_no);

  // Add the can_recover argument to the (now) new bindings, and
  // attach it to any recover statements.
  Variable* can_recover_var = new Variable(Type::lookup_bool_type(), NULL,
					   false, true, false, location);
  can_recover_no = new_bindings->add_variable(can_recover_name, NULL,
					      can_recover_var);
  Convert_recover convert_recover(can_recover_no);
  new_func->traverse(&convert_recover);

  // Update the function pointers in any named results.
  new_func->update_result_variables();
  orig_func->update_result_variables();

  return TRAVERSE_CONTINUE;
}

// Return the expression to pass for the .can_recover parameter to the
// new function.  This indicates whether a call to recover may return
// non-nil.  The expression is runtime.canrecover(__builtin_return_address()).

Expression*
Build_recover_thunks::can_recover_arg(Location location)
{
  Type* uintptr_type = Type::lookup_integer_type("uintptr");
  static Named_object* can_recover;
  if (can_recover == NULL)
    {
      const Location bloc = Linemap::predeclared_location();
      Typed_identifier_list* param_types = new Typed_identifier_list();
      param_types->push_back(Typed_identifier("a", uintptr_type, bloc));
      Type* boolean_type = Type::lookup_bool_type();
      Typed_identifier_list* results = new Typed_identifier_list();
      results->push_back(Typed_identifier("", boolean_type, bloc));
      Function_type* fntype = Type::make_function_type(NULL, param_types,
						       results, bloc);
      can_recover =
	Named_object::make_function_declaration("runtime_canrecover",
						NULL, fntype, bloc);
      can_recover->func_declaration_value()->set_asm_name("runtime.canrecover");
    }

  Expression* zexpr = Expression::make_integer_ul(0, NULL, location);
  Expression* call = Runtime::make_call(Runtime::BUILTIN_RETURN_ADDRESS,
                                        location, 1, zexpr);
  call = Expression::make_unsafe_cast(uintptr_type, call, location);

  Expression_list* args = new Expression_list();
  args->push_back(call);

  Expression* fn = Expression::make_func_reference(can_recover, NULL, location);
  return Expression::make_call(fn, args, false, location);
}

// Build thunks for functions which call recover.  We build a new
// function with an extra parameter, which is whether a call to
// recover can succeed.  We then move the body of this function to
// that one.  We then turn this function into a thunk which calls the
// new one, passing the value of runtime.canrecover(__builtin_return_address()).
// The function will be marked as not splitting the stack.  This will
// cooperate with the implementation of defer to make recover do the
// right thing.

void
Gogo::build_recover_thunks()
{
  Build_recover_thunks build_recover_thunks(this);
  this->traverse(&build_recover_thunks);
}

// Look for named types to see whether we need to create an interface
// method table.

class Build_method_tables : public Traverse
{
 public:
  Build_method_tables(Gogo* gogo,
		      const std::vector<Interface_type*>& interfaces)
    : Traverse(traverse_types),
      gogo_(gogo), interfaces_(interfaces)
  { }

  int
  type(Type*);

 private:
  // The IR.
  Gogo* gogo_;
  // A list of locally defined interfaces which have hidden methods.
  const std::vector<Interface_type*>& interfaces_;
};

// Build all required interface method tables for types.  We need to
// ensure that we have an interface method table for every interface
// which has a hidden method, for every named type which implements
// that interface.  Normally we can just build interface method tables
// as we need them.  However, in some cases we can require an
// interface method table for an interface defined in a different
// package for a type defined in that package.  If that interface and
// type both use a hidden method, that is OK.  However, we will not be
// able to build that interface method table when we need it, because
// the type's hidden method will be static.  So we have to build it
// here, and just refer it from other packages as needed.

void
Gogo::build_interface_method_tables()
{
  if (saw_errors())
    return;

  std::vector<Interface_type*> hidden_interfaces;
  hidden_interfaces.reserve(this->interface_types_.size());
  for (std::vector<Interface_type*>::const_iterator pi =
	 this->interface_types_.begin();
       pi != this->interface_types_.end();
       ++pi)
    {
      const Typed_identifier_list* methods = (*pi)->methods();
      if (methods == NULL)
	continue;
      for (Typed_identifier_list::const_iterator pm = methods->begin();
	   pm != methods->end();
	   ++pm)
	{
	  if (Gogo::is_hidden_name(pm->name()))
	    {
	      hidden_interfaces.push_back(*pi);
	      break;
	    }
	}
    }

  if (!hidden_interfaces.empty())
    {
      // Now traverse the tree looking for all named types.
      Build_method_tables bmt(this, hidden_interfaces);
      this->traverse(&bmt);
    }

  // We no longer need the list of interfaces.

  this->interface_types_.clear();
}

// This is called for each type.  For a named type, for each of the
// interfaces with hidden methods that it implements, create the
// method table.

int
Build_method_tables::type(Type* type)
{
  Named_type* nt = type->named_type();
  Struct_type* st = type->struct_type();
  if (nt != NULL || st != NULL)
    {
      Translate_context context(this->gogo_, NULL, NULL, NULL);
      for (std::vector<Interface_type*>::const_iterator p =
	     this->interfaces_.begin();
	   p != this->interfaces_.end();
	   ++p)
	{
	  // We ask whether a pointer to the named type implements the
	  // interface, because a pointer can implement more methods
	  // than a value.
	  if (nt != NULL)
	    {
	      if ((*p)->implements_interface(Type::make_pointer_type(nt),
					     NULL))
		{
		  nt->interface_method_table(*p, false)->get_backend(&context);
                  nt->interface_method_table(*p, true)->get_backend(&context);
		}
	    }
	  else
	    {
	      if ((*p)->implements_interface(Type::make_pointer_type(st),
					     NULL))
		{
		  st->interface_method_table(*p, false)->get_backend(&context);
		  st->interface_method_table(*p, true)->get_backend(&context);
		}
	    }
	}
    }
  return TRAVERSE_CONTINUE;
}

// Return an expression which allocates memory to hold values of type TYPE.

Expression*
Gogo::allocate_memory(Type* type, Location location)
{
  Expression* td = Expression::make_type_descriptor(type, location);
  return Runtime::make_call(Runtime::NEW, location, 1, td);
}

// Traversal class used to check for return statements.

class Check_return_statements_traverse : public Traverse
{
 public:
  Check_return_statements_traverse()
    : Traverse(traverse_functions)
  { }

  int
  function(Named_object*);
};

// Check that a function has a return statement if it needs one.

int
Check_return_statements_traverse::function(Named_object* no)
{
  Function* func = no->func_value();
  const Function_type* fntype = func->type();
  const Typed_identifier_list* results = fntype->results();

  // We only need a return statement if there is a return value.
  if (results == NULL || results->empty())
    return TRAVERSE_CONTINUE;

  if (func->block()->may_fall_through())
    go_error_at(func->block()->end_location(),
		"missing return at end of function");

  return TRAVERSE_CONTINUE;
}

// Check return statements.

void
Gogo::check_return_statements()
{
  Check_return_statements_traverse traverse;
  this->traverse(&traverse);
}

// Traversal class to decide whether a function body is less than the
// inlining budget.  This adjusts *available as it goes, and stops the
// traversal if it goes negative.

class Inline_within_budget : public Traverse
{
 public:
  Inline_within_budget(int* available)
    : Traverse(traverse_statements
	       | traverse_expressions),
      available_(available)
  { }

  int
  statement(Block*, size_t*, Statement*);

  int
  expression(Expression**);

 private:
  // Pointer to remaining budget.
  int* available_;
};

// Adjust the budget for the inlining cost of a statement.

int
Inline_within_budget::statement(Block*, size_t*, Statement* s)
{
  if (*this->available_ < 0)
    return TRAVERSE_EXIT;
  *this->available_ -= s->inlining_cost();
  return TRAVERSE_CONTINUE;
}

// Adjust the budget for the inlining cost of an expression.

int
Inline_within_budget::expression(Expression** pexpr)
{
  if (*this->available_ < 0)
    return TRAVERSE_EXIT;
  *this->available_ -= (*pexpr)->inlining_cost();
  return TRAVERSE_CONTINUE;
}

// Traversal class to find functions whose body should be exported for
// inlining by other packages.

class Mark_inline_candidates : public Traverse
{
 public:
  Mark_inline_candidates(Unordered_set(Named_object*)* marked)
    : Traverse(traverse_functions
	       | traverse_types),
      marked_functions_(marked)
  { }

  int
  function(Named_object*);

  int
  type(Type*);

 private:
  // We traverse the function body trying to determine how expensive
  // it is for inlining.  We start with a budget, and decrease that
  // budget for each statement and expression.  If the budget goes
  // negative, we do not export the function body.  The value of this
  // budget is a heuristic.  In the usual GCC spirit, we could
  // consider setting this via a command line option.
  const int budget_heuristic = 80;

  // Set of named objects that are marked as inline candidates.
  Unordered_set(Named_object*)* marked_functions_;
};

// Mark a function if it is an inline candidate.

int
Mark_inline_candidates::function(Named_object* no)
{
  Function* func = no->func_value();
  if ((func->pragmas() & GOPRAGMA_NOINLINE) != 0)
    return TRAVERSE_CONTINUE;
  int budget = budget_heuristic;
  Inline_within_budget iwb(&budget);
  func->block()->traverse(&iwb);
  if (budget >= 0)
    {
      func->set_export_for_inlining();
      this->marked_functions_->insert(no);
    }
  return TRAVERSE_CONTINUE;
}

// Mark methods if they are inline candidates.

int
Mark_inline_candidates::type(Type* t)
{
  Named_type* nt = t->named_type();
  if (nt == NULL || nt->is_alias())
    return TRAVERSE_CONTINUE;
  const Bindings* methods = nt->local_methods();
  if (methods == NULL)
    return TRAVERSE_CONTINUE;
  for (Bindings::const_definitions_iterator p = methods->begin_definitions();
       p != methods->end_definitions();
       ++p)
    {
      Named_object* no = *p;
      go_assert(no->is_function());
      Function *func = no->func_value();
      if ((func->pragmas() & GOPRAGMA_NOINLINE) != 0)
        continue;
      int budget = budget_heuristic;
      Inline_within_budget iwb(&budget);
      func->block()->traverse(&iwb);
      if (budget >= 0)
        {
          func->set_export_for_inlining();
          this->marked_functions_->insert(no);
        }
    }
  return TRAVERSE_CONTINUE;
}

// Export identifiers as requested.

void
Gogo::do_exports()
{
  if (saw_errors())
    return;

  // Mark any functions whose body should be exported for inlining by
  // other packages.
  Unordered_set(Named_object*) marked_functions;
  Mark_inline_candidates mic(&marked_functions);
  this->traverse(&mic);

  // For now we always stream to a section.  Later we may want to
  // support streaming to a separate file.
  Stream_to_section stream(this->backend());

  // Write out either the prefix or pkgpath depending on how we were
  // invoked.
  std::string prefix;
  std::string pkgpath;
  if (this->pkgpath_from_option_)
    pkgpath = this->pkgpath_;
  else if (this->prefix_from_option_)
    prefix = this->prefix_;
  else if (this->is_main_package())
    pkgpath = "main";
  else
    prefix = "go";

  std::string init_fn_name;
  if (this->is_main_package())
    init_fn_name = "";
  else if (this->need_init_fn_)
    init_fn_name = this->get_init_fn_name();
  else
    init_fn_name = this->dummy_init_fn_name();

  Export exp(&stream);
  exp.register_builtin_types(this);
  exp.export_globals(this->package_name(),
		     prefix,
		     pkgpath,
		     this->packages_,
		     this->imports_,
		     init_fn_name,
		     this->imported_init_fns_,
		     this->package_->bindings(),
                     &marked_functions);

  if (!this->c_header_.empty() && !saw_errors())
    this->write_c_header();
}

// Write the top level named struct types in C format to a C header
// file.  This is used when building the runtime package, to share
// struct definitions between C and Go.

void
Gogo::write_c_header()
{
  std::ofstream out;
  out.open(this->c_header_.c_str());
  if (out.fail())
    {
      go_error_at(Linemap::unknown_location(),
		  "cannot open %s: %m", this->c_header_.c_str());
      return;
    }

  std::list<Named_object*> types;
  Bindings* top = this->package_->bindings();
  for (Bindings::const_definitions_iterator p = top->begin_definitions();
       p != top->end_definitions();
       ++p)
    {
      Named_object* no = *p;

      // Skip names that start with underscore followed by something
      // other than an uppercase letter, as when compiling the runtime
      // package they are mostly types defined by mkrsysinfo.sh based
      // on the C system header files.  We don't need to translate
      // types to C and back to Go.  But do accept the special cases
      // _defer, _panic, and _type.
      std::string name = Gogo::unpack_hidden_name(no->name());
      if (name[0] == '_'
	  && (name[1] < 'A' || name[1] > 'Z')
	  && (name != "_defer" && name != "_panic" && name != "_type"))
	continue;

      if (no->is_type() && no->type_value()->struct_type() != NULL)
	types.push_back(no);
      if (no->is_const()
	  && no->const_value()->type()->integer_type() != NULL
	  && !no->const_value()->is_sink())
	{
	  Numeric_constant nc;
	  unsigned long val;
	  if (no->const_value()->expr()->numeric_constant_value(&nc)
	      && nc.to_unsigned_long(&val) == Numeric_constant::NC_UL_VALID)
	    {
	      out << "#define " << no->message_name() << ' ' << val
		  << std::endl;
	    }
	}
    }

  std::vector<const Named_object*> written;
  int loop = 0;
  while (!types.empty())
    {
      Named_object* no = types.front();
      types.pop_front();

      std::vector<const Named_object*> needs;
      std::vector<const Named_object*> declare;
      if (!no->type_value()->struct_type()->can_write_to_c_header(&needs,
								  &declare))
	continue;

      bool ok = true;
      for (std::vector<const Named_object*>::const_iterator pr
	     = needs.begin();
	   pr != needs.end() && ok;
	   ++pr)
	{
	  for (std::list<Named_object*>::const_iterator pt = types.begin();
	       pt != types.end() && ok;
	       ++pt)
	    if (*pr == *pt)
	      ok = false;
	}
      if (!ok)
	{
	  ++loop;
	  if (loop > 10000)
	    {
	      // This should be impossible since the code parsed and
	      // type checked.
	      go_unreachable();
	    }

	  types.push_back(no);
	  continue;
	}

      for (std::vector<const Named_object*>::const_iterator pd
	     = declare.begin();
	   pd != declare.end();
	   ++pd)
	{
	  if (*pd == no)
	    continue;

	  std::vector<const Named_object*> dneeds;
	  std::vector<const Named_object*> ddeclare;
	  if (!(*pd)->type_value()->struct_type()->
	      can_write_to_c_header(&dneeds, &ddeclare))
	    continue;

	  bool done = false;
	  for (std::vector<const Named_object*>::const_iterator pw
		 = written.begin();
	       pw != written.end();
	       ++pw)
	    {
	      if (*pw == *pd)
		{
		  done = true;
		  break;
		}
	    }
	  if (!done)
	    {
	      out << std::endl;
	      out << "struct " << (*pd)->message_name() << ";" << std::endl;
	      written.push_back(*pd);
	    }
	}

      out << std::endl;
      out << "struct " << no->message_name() << " {" << std::endl;
      no->type_value()->struct_type()->write_to_c_header(out);
      out << "};" << std::endl;
      written.push_back(no);
    }

  out.close();
  if (out.fail())
    go_error_at(Linemap::unknown_location(),
		"error writing to %s: %m", this->c_header_.c_str());
}

// Find the blocks in order to convert named types defined in blocks.

class Convert_named_types : public Traverse
{
 public:
  Convert_named_types(Gogo* gogo)
    : Traverse(traverse_blocks),
      gogo_(gogo)
  { }

 protected:
  int
  block(Block* block);

 private:
  Gogo* gogo_;
};

int
Convert_named_types::block(Block* block)
{
  this->gogo_->convert_named_types_in_bindings(block->bindings());
  return TRAVERSE_CONTINUE;
}

// Convert all named types to the backend representation.  Since named
// types can refer to other types, this needs to be done in the right
// sequence, which is handled by Named_type::convert.  Here we arrange
// to call that for each named type.

void
Gogo::convert_named_types()
{
  this->convert_named_types_in_bindings(this->globals_);
  for (Packages::iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    {
      Package* package = p->second;
      this->convert_named_types_in_bindings(package->bindings());
    }

  Convert_named_types cnt(this);
  this->traverse(&cnt);

  // Make all the builtin named types used for type descriptors, and
  // then convert them.  They will only be written out if they are
  // needed.
  Type::make_type_descriptor_type();
  Type::make_type_descriptor_ptr_type();
  Function_type::make_function_type_descriptor_type();
  Pointer_type::make_pointer_type_descriptor_type();
  Struct_type::make_struct_type_descriptor_type();
  Array_type::make_array_type_descriptor_type();
  Array_type::make_slice_type_descriptor_type();
  Map_type::make_map_type_descriptor_type();
  Channel_type::make_chan_type_descriptor_type();
  Interface_type::make_interface_type_descriptor_type();
  Expression::make_func_descriptor_type();
  Type::convert_builtin_named_types(this);

  Runtime::convert_types(this);

  this->named_types_are_converted_ = true;

  Type::finish_pointer_types(this);
}

// Convert all names types in a set of bindings.

void
Gogo::convert_named_types_in_bindings(Bindings* bindings)
{
  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p)
    {
      if ((*p)->is_type())
	(*p)->type_value()->convert(this);
    }
}

void
debug_go_gogo(Gogo* gogo)
{
  if (gogo != NULL)
    gogo->debug_dump();
}

void
Gogo::debug_dump()
{
  std::cerr << "Packages:\n";
  for (Packages::const_iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    {
      const char *tag = "  ";
      if (p->second == this->package_)
        tag = "* ";
      std::cerr << tag << "'" << p->first << "' "
                << p->second->pkgpath() << " " << ((void*)p->second) << "\n";
    }
}

// Class Function.

Function::Function(Function_type* type, Named_object* enclosing, Block* block,
		   Location location)
  : type_(type), enclosing_(enclosing), results_(NULL),
    closure_var_(NULL), block_(block), location_(location), labels_(),
    local_type_count_(0), descriptor_(NULL), fndecl_(NULL), defer_stack_(NULL),
    pragmas_(0), nested_functions_(0), is_sink_(false),
    results_are_named_(false), is_unnamed_type_stub_method_(false),
    calls_recover_(false), is_recover_thunk_(false), has_recover_thunk_(false),
    calls_defer_retaddr_(false), is_type_specific_function_(false),
    in_unique_section_(false), export_for_inlining_(false),
    is_inline_only_(false), is_referenced_by_inline_(false),
    is_exported_by_linkname_(false)
{
}

// Create the named result variables.

void
Function::create_result_variables(Gogo* gogo)
{
  const Typed_identifier_list* results = this->type_->results();
  if (results == NULL || results->empty())
    return;

  if (!results->front().name().empty())
    this->results_are_named_ = true;

  this->results_ = new Results();
  this->results_->reserve(results->size());

  Block* block = this->block_;
  int index = 0;
  for (Typed_identifier_list::const_iterator p = results->begin();
       p != results->end();
       ++p, ++index)
    {
      std::string name = p->name();
      if (name.empty() || Gogo::is_sink_name(name))
	{
	  static int result_counter;
	  char buf[100];
	  snprintf(buf, sizeof buf, "$ret%d", result_counter);
	  ++result_counter;
	  name = gogo->pack_hidden_name(buf, false);
	}
      Result_variable* result = new Result_variable(p->type(), this, index,
						    p->location());
      Named_object* no = block->bindings()->add_result_variable(name, result);
      if (no->is_result_variable())
	this->results_->push_back(no);
      else
	{
	  static int dummy_result_count;
	  char buf[100];
	  snprintf(buf, sizeof buf, "$dret%d", dummy_result_count);
	  ++dummy_result_count;
	  name = gogo->pack_hidden_name(buf, false);
	  no = block->bindings()->add_result_variable(name, result);
	  go_assert(no->is_result_variable());
	  this->results_->push_back(no);
	}
    }
}

// Update the named result variables when cloning a function which
// calls recover.

void
Function::update_result_variables()
{
  if (this->results_ == NULL)
    return;

  for (Results::iterator p = this->results_->begin();
       p != this->results_->end();
       ++p)
    (*p)->result_var_value()->set_function(this);
}

// Whether this method should not be included in the type descriptor.

bool
Function::nointerface() const
{
  go_assert(this->is_method());
  return (this->pragmas_ & GOPRAGMA_NOINTERFACE) != 0;
}

// Record that this method should not be included in the type
// descriptor.

void
Function::set_nointerface()
{
  this->pragmas_ |= GOPRAGMA_NOINTERFACE;
}

// Return the closure variable, creating it if necessary.

Named_object*
Function::closure_var()
{
  if (this->closure_var_ == NULL)
    {
      go_assert(this->descriptor_ == NULL);
      // We don't know the type of the variable yet.  We add fields as
      // we find them.
      Location loc = this->type_->location();
      Struct_field_list* sfl = new Struct_field_list;
      Struct_type* struct_type = Type::make_struct_type(sfl, loc);
      struct_type->set_is_struct_incomparable();
      Variable* var = new Variable(Type::make_pointer_type(struct_type),
				   NULL, false, false, false, loc);
      var->set_is_used();
      var->set_is_closure();
      this->closure_var_ = Named_object::make_variable("$closure", NULL, var);
      // Note that the new variable is not in any binding contour.
    }
  return this->closure_var_;
}

// Set the type of the closure variable.

void
Function::set_closure_type()
{
  if (this->closure_var_ == NULL)
    return;
  Named_object* closure = this->closure_var_;
  Struct_type* st = closure->var_value()->type()->deref()->struct_type();

  // The first field of a closure is always a pointer to the function
  // code.
  Type* voidptr_type = Type::make_pointer_type(Type::make_void_type());
  st->push_field(Struct_field(Typed_identifier(".f", voidptr_type,
					       this->location_)));

  unsigned int index = 1;
  for (Closure_fields::const_iterator p = this->closure_fields_.begin();
       p != this->closure_fields_.end();
       ++p, ++index)
    {
      Named_object* no = p->first;
      char buf[20];
      snprintf(buf, sizeof buf, "%u", index);
      std::string n = no->name() + buf;
      Type* var_type;
      if (no->is_variable())
	var_type = no->var_value()->type();
      else
	var_type = no->result_var_value()->type();
      Type* field_type = Type::make_pointer_type(var_type);
      st->push_field(Struct_field(Typed_identifier(n, field_type, p->second)));
    }
}

// Return whether this function is a method.

bool
Function::is_method() const
{
  return this->type_->is_method();
}

// Add a label definition.

Label*
Function::add_label_definition(Gogo* gogo, const std::string& label_name,
			       Location location)
{
  Label* lnull = NULL;
  std::pair<Labels::iterator, bool> ins =
    this->labels_.insert(std::make_pair(label_name, lnull));
  Label* label;
  if (label_name == "_")
    {
      label = Label::create_dummy_label();
      if (ins.second)
	ins.first->second = label;
    }
  else if (ins.second)
    {
      // This is a new label.
      label = new Label(label_name);
      ins.first->second = label;
    }
  else
    {
      // The label was already in the hash table.
      label = ins.first->second;
      if (label->is_defined())
	{
	  go_error_at(location, "label %qs already defined",
		      Gogo::message_name(label_name).c_str());
	  go_inform(label->location(), "previous definition of %qs was here",
		    Gogo::message_name(label_name).c_str());
	  return new Label(label_name);
	}
    }

  label->define(location, gogo->bindings_snapshot(location));

  // Issue any errors appropriate for any previous goto's to this
  // label.
  const std::vector<Bindings_snapshot*>& refs(label->refs());
  for (std::vector<Bindings_snapshot*>::const_iterator p = refs.begin();
       p != refs.end();
       ++p)
    (*p)->check_goto_to(gogo->current_block());
  label->clear_refs();

  return label;
}

// Add a reference to a label.

Label*
Function::add_label_reference(Gogo* gogo, const std::string& label_name,
			      Location location, bool issue_goto_errors)
{
  Label* lnull = NULL;
  std::pair<Labels::iterator, bool> ins =
    this->labels_.insert(std::make_pair(label_name, lnull));
  Label* label;
  if (!ins.second)
    {
      // The label was already in the hash table.
      label = ins.first->second;
    }
  else
    {
      go_assert(ins.first->second == NULL);
      label = new Label(label_name);
      ins.first->second = label;
    }

  label->set_is_used();

  if (issue_goto_errors)
    {
      Bindings_snapshot* snapshot = label->snapshot();
      if (snapshot != NULL)
	snapshot->check_goto_from(gogo->current_block(), location);
      else
	label->add_snapshot_ref(gogo->bindings_snapshot(location));
    }

  return label;
}

// Warn about labels that are defined but not used.

void
Function::check_labels() const
{
  for (Labels::const_iterator p = this->labels_.begin();
       p != this->labels_.end();
       p++)
    {
      Label* label = p->second;
      if (!label->is_used())
	go_error_at(label->location(), "label %qs defined and not used",
		    Gogo::message_name(label->name()).c_str());
    }
}

// Set the receiver type.  This is used to remove aliases.

void
Function::set_receiver_type(Type* rtype)
{
  Function_type* oft = this->type_;
  Typed_identifier* rec = new Typed_identifier(oft->receiver()->name(),
					       rtype,
					       oft->receiver()->location());
  Typed_identifier_list* parameters = NULL;
  if (oft->parameters() != NULL)
    parameters = oft->parameters()->copy();
  Typed_identifier_list* results = NULL;
  if (oft->results() != NULL)
    results = oft->results()->copy();
  Function_type* nft = Type::make_function_type(rec, parameters, results,
						oft->location());
  this->type_ = nft;
}

// Swap one function with another.  This is used when building the
// thunk we use to call a function which calls recover.  It may not
// work for any other case.

void
Function::swap_for_recover(Function *x)
{
  go_assert(this->enclosing_ == x->enclosing_);
  std::swap(this->results_, x->results_);
  std::swap(this->closure_var_, x->closure_var_);
  std::swap(this->block_, x->block_);
  go_assert(this->location_ == x->location_);
  go_assert(this->fndecl_ == NULL && x->fndecl_ == NULL);
  go_assert(this->defer_stack_ == NULL && x->defer_stack_ == NULL);
}

// Traverse the tree.

int
Function::traverse(Traverse* traverse)
{
  unsigned int traverse_mask = traverse->traverse_mask();

  if ((traverse_mask
       & (Traverse::traverse_types | Traverse::traverse_expressions))
      != 0)
    {
      if (Type::traverse(this->type_, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }

  // FIXME: We should check traverse_functions here if nested
  // functions are stored in block bindings.
  if (this->block_ != NULL
      && (traverse_mask
	  & (Traverse::traverse_variables
	     | Traverse::traverse_constants
	     | Traverse::traverse_blocks
	     | Traverse::traverse_statements
	     | Traverse::traverse_expressions
	     | Traverse::traverse_types)) != 0)
    {
      if (this->block_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }

  return TRAVERSE_CONTINUE;
}

// Work out types for unspecified variables and constants.

void
Function::determine_types()
{
  if (this->block_ != NULL)
    this->block_->determine_types();
}

// Return the function descriptor, the value you get when you refer to
// the function in Go code without calling it.

Expression*
Function::descriptor(Gogo*, Named_object* no)
{
  go_assert(!this->is_method());
  go_assert(this->closure_var_ == NULL);
  if (this->descriptor_ == NULL)
    this->descriptor_ = Expression::make_func_descriptor(no);
  return this->descriptor_;
}

// Get a pointer to the variable representing the defer stack for this
// function, making it if necessary.  The value of the variable is set
// by the runtime routines to true if the function is returning,
// rather than panicing through.  A pointer to this variable is used
// as a marker for the functions on the defer stack associated with
// this function.  A function-specific variable permits inlining a
// function which uses defer.

Expression*
Function::defer_stack(Location location)
{
  if (this->defer_stack_ == NULL)
    {
      Type* t = Type::lookup_bool_type();
      Expression* n = Expression::make_boolean(false, location);
      this->defer_stack_ = Statement::make_temporary(t, n, location);
      this->defer_stack_->set_is_address_taken();
    }
  Expression* ref = Expression::make_temporary_reference(this->defer_stack_,
							 location);
  return Expression::make_unary(OPERATOR_AND, ref, location);
}

// Export the function.

void
Function::export_func(Export* exp, const Named_object* no) const
{
  Block* block = NULL;
  if (this->export_for_inlining())
    block = this->block_;
  Function::export_func_with_type(exp, no, this->type_, this->results_,
				  this->is_method() && this->nointerface(),
				  this->asm_name(), block, this->location_);
}

// Export a function with a type.

void
Function::export_func_with_type(Export* exp, const Named_object* no,
				const Function_type* fntype,
				Function::Results* result_vars,
				bool nointerface, const std::string& asm_name,
				Block* block, Location loc)
{
  exp->write_c_string("func ");

  if (nointerface)
    {
      go_assert(fntype->is_method());
      exp->write_c_string("/*nointerface*/ ");
    }

  if (!asm_name.empty())
    {
      exp->write_c_string("/*asm ");
      exp->write_string(asm_name);
      exp->write_c_string(" */ ");
    }

  if (fntype->is_method())
    {
      exp->write_c_string("(");
      const Typed_identifier* receiver = fntype->receiver();
      exp->write_name(receiver->name());
      exp->write_escape(receiver->note());
      exp->write_c_string(" ");
      exp->write_type(receiver->type()->unalias());
      exp->write_c_string(") ");
    }

  if (no->package() != NULL && !fntype->is_method())
    {
      char buf[50];
      snprintf(buf, sizeof buf, "<p%d>", exp->package_index(no->package()));
      exp->write_c_string(buf);
    }

  const std::string& name(no->name());
  if (!Gogo::is_hidden_name(name))
    exp->write_string(name);
  else
    {
      exp->write_c_string(".");
      exp->write_string(Gogo::unpack_hidden_name(name));
    }

  exp->write_c_string(" (");
  const Typed_identifier_list* parameters = fntype->parameters();
  if (parameters != NULL)
    {
      size_t i = 0;
      bool is_varargs = fntype->is_varargs();
      bool first = true;
      for (Typed_identifier_list::const_iterator p = parameters->begin();
	   p != parameters->end();
	   ++p, ++i)
	{
	  if (first)
	    first = false;
	  else
	    exp->write_c_string(", ");
	  exp->write_name(p->name());
	  exp->write_escape(p->note());
	  exp->write_c_string(" ");
	  if (!is_varargs || p + 1 != parameters->end())
	    exp->write_type(p->type());
	  else
	    {
	      exp->write_c_string("...");
	      exp->write_type(p->type()->array_type()->element_type());
	    }
	}
    }
  exp->write_c_string(")");

  const Typed_identifier_list* result_decls = fntype->results();
  if (result_decls != NULL)
    {
      if (result_decls->size() == 1
	  && result_decls->begin()->name().empty()
	  && block == NULL)
	{
	  exp->write_c_string(" ");
	  exp->write_type(result_decls->begin()->type());
	}
      else
	{
	  exp->write_c_string(" (");
	  bool first = true;
	  Results::const_iterator pr;
	  if (result_vars != NULL)
	    pr = result_vars->begin();
	  for (Typed_identifier_list::const_iterator pd = result_decls->begin();
	       pd != result_decls->end();
	       ++pd)
	    {
	      if (first)
		first = false;
	      else
		exp->write_c_string(", ");
	      // We only use pr->name, which may be artificial, if
	      // need it for inlining.
	      if (block == NULL || result_vars == NULL)
		exp->write_name(pd->name());
	      else
		exp->write_name((*pr)->name());
	      exp->write_escape(pd->note());
	      exp->write_c_string(" ");
	      exp->write_type(pd->type());
	      if (result_vars != NULL)
		++pr;
	    }
	  if (result_vars != NULL)
	    go_assert(pr == result_vars->end());
	  exp->write_c_string(")");
	}
    }

  if (block == NULL)
    exp->write_c_string("\n");
  else
    {
      int indent = 1;
      if (fntype->is_method())
	indent++;

      Export_function_body efb(exp, indent);

      efb.indent();
      efb.write_c_string("// ");
      efb.write_string(Linemap::location_to_file(block->start_location()));
      efb.write_char(':');
      char buf[100];
      snprintf(buf, sizeof buf, "%d", Linemap::location_to_line(loc));
      efb.write_c_string(buf);
      efb.write_char('\n');
      block->export_block(&efb);

      const std::string& body(efb.body());

      snprintf(buf, sizeof buf, " <inl:%lu>\n",
	       static_cast<unsigned long>(body.length()));
      exp->write_c_string(buf);

      exp->write_string(body);
    }
}

// Import a function.

bool
Function::import_func(Import* imp, std::string* pname,
		      Package** ppkg, bool* pis_exported,
		      Typed_identifier** preceiver,
		      Typed_identifier_list** pparameters,
		      Typed_identifier_list** presults,
		      bool* is_varargs,
		      bool* nointerface,
		      std::string* asm_name,
		      std::string* body)
{
  imp->require_c_string("func ");

  *nointerface = false;
  while (imp->match_c_string("/*"))
    {
      imp->advance(2);
      if (imp->match_c_string("nointerface"))
	{
	  imp->require_c_string("nointerface*/ ");
	  *nointerface = true;
	}
      else if (imp->match_c_string("asm"))
	{
	  imp->require_c_string("asm ");
	  *asm_name = imp->read_identifier();
	  imp->require_c_string(" */ ");
	}
      else
	{
	  go_error_at(imp->location(),
		      "import error at %d: unrecognized function comment",
		      imp->pos());
	  return false;
	}
    }

  if (*nointerface)
    {
      // Only a method can be nointerface.
      go_assert(imp->peek_char() == '(');
    }

  *preceiver = NULL;
  if (imp->peek_char() == '(')
    {
      imp->require_c_string("(");
      std::string name = imp->read_name();
      std::string escape_note = imp->read_escape();
      imp->require_c_string(" ");
      Type* rtype = imp->read_type();
      *preceiver = new Typed_identifier(name, rtype, imp->location());
      (*preceiver)->set_note(escape_note);
      imp->require_c_string(") ");
    }

  if (!Import::read_qualified_identifier(imp, pname, ppkg, pis_exported))
    {
      go_error_at(imp->location(),
		  "import error at %d: bad function name in export data",
		  imp->pos());
      return false;
    }

  Typed_identifier_list* parameters;
  *is_varargs = false;
  imp->require_c_string(" (");
  if (imp->peek_char() == ')')
    parameters = NULL;
  else
    {
      parameters = new Typed_identifier_list();
      while (true)
	{
	  std::string name = imp->read_name();
	  std::string escape_note = imp->read_escape();
	  imp->require_c_string(" ");

	  if (imp->match_c_string("..."))
	    {
	      imp->advance(3);
	      *is_varargs = true;
	    }

	  Type* ptype = imp->read_type();
	  if (*is_varargs)
	    ptype = Type::make_array_type(ptype, NULL);
	  Typed_identifier t = Typed_identifier(name, ptype, imp->location());
	  t.set_note(escape_note);
	  parameters->push_back(t);
	  if (imp->peek_char() != ',')
	    break;
	  go_assert(!*is_varargs);
	  imp->require_c_string(", ");
	}
    }
  imp->require_c_string(")");
  *pparameters = parameters;

  Typed_identifier_list* results;
  if (imp->peek_char() != ' ' || imp->match_c_string(" <inl"))
    results = NULL;
  else
    {
      results = new Typed_identifier_list();
      imp->require_c_string(" ");
      if (imp->peek_char() != '(')
	{
	  Type* rtype = imp->read_type();
	  results->push_back(Typed_identifier("", rtype, imp->location()));
	}
      else
	{
	  imp->require_c_string("(");
	  while (true)
	    {
	      std::string name = imp->read_name();
	      std::string note = imp->read_escape();
	      imp->require_c_string(" ");
	      Type* rtype = imp->read_type();
	      Typed_identifier t = Typed_identifier(name, rtype,
						    imp->location());
	      t.set_note(note);
	      results->push_back(t);
	      if (imp->peek_char() != ',')
		break;
	      imp->require_c_string(", ");
	    }
	  imp->require_c_string(")");
	}
    }
  *presults = results;

  if (!imp->match_c_string(" <inl:"))
    {
      imp->require_semicolon_if_old_version();
      imp->require_c_string("\n");
      body->clear();
    }
  else
    {
      imp->require_c_string(" <inl:");
      std::string lenstr;
      int c;
      while (true)
	{
	  c = imp->peek_char();
	  if (c < '0' || c > '9')
	    break;
	  lenstr += c;
	  imp->get_char();
	}
      imp->require_c_string(">\n");

      errno = 0;
      char* end;
      long llen = strtol(lenstr.c_str(), &end, 10);
      if (*end != '\0'
	  || llen < 0
	  || (llen == LONG_MAX && errno == ERANGE))
	{
	  go_error_at(imp->location(), "invalid inline function length %s",
		      lenstr.c_str());
	  return false;
	}

      imp->read(static_cast<size_t>(llen), body);
    }

  return true;
}

// Get the backend name.

void
Function::backend_name(Gogo* gogo, Named_object* no, Backend_name *bname)
{
  if (!this->asm_name_.empty())
    bname->set_asm_name(this->asm_name_);
  else if (no->package() == NULL && no->name() == gogo->get_init_fn_name())
    {
      // These names appear in the export data and are used
      // directly in the assembler code.  If we change this here
      // we need to change Gogo::init_imports.
      bname->set_asm_name(no->name());
    }
  else if (this->enclosing_ != NULL)
    {
      // Rewrite the nested name to use the enclosing function name.
      // We don't do this earlier because we just store simple names
      // in a Named_object, not Backend_names.

      // The name was set by nested_function_name, which always
      // appends ..funcNNN.  We want that to be our suffix.
      size_t pos = no->name().find("..func");
      go_assert(pos != std::string::npos);

      Named_object* enclosing = this->enclosing_;
      while (true)
	{
	  Named_object* parent = enclosing->func_value()->enclosing();
	  if (parent == NULL)
	    break;
	  enclosing = parent;
	}

      Type* rtype = NULL;
      if (enclosing->func_value()->type()->is_method())
	rtype = enclosing->func_value()->type()->receiver()->type();
      gogo->function_backend_name(enclosing->name(), enclosing->package(),
				  rtype, bname);
      bname->append_suffix(no->name().substr(pos));
    }
  else
    {
      Type* rtype = NULL;
      if (this->type_->is_method())
	rtype = this->type_->receiver()->type();
      gogo->function_backend_name(no->name(), no->package(), rtype, bname);
    }
}

// Get the backend representation.

Bfunction*
Function::get_or_make_decl(Gogo* gogo, Named_object* no)
{
  if (this->fndecl_ == NULL)
    {
      unsigned int flags = 0;
      if (no->package() != NULL)
	{
	  // Functions defined in other packages must be visible.
	  flags |= Backend::function_is_visible;
	}
      else if (this->enclosing_ != NULL || Gogo::is_thunk(no))
        ;
      else if (Gogo::unpack_hidden_name(no->name()) == "init"
               && !this->type_->is_method())
	;
      else if (no->name() == gogo->get_init_fn_name())
	flags |= Backend::function_is_visible;
      else if (Gogo::unpack_hidden_name(no->name()) == "main"
               && gogo->is_main_package())
	flags |= Backend::function_is_visible;
      // Methods have to be public even if they are hidden because
      // they can be pulled into type descriptors when using
      // anonymous fields.
      else if (!Gogo::is_hidden_name(no->name())
               || this->type_->is_method())
        {
	  if (!this->is_unnamed_type_stub_method_)
	    flags |= Backend::function_is_visible;
        }

      if (!this->asm_name_.empty())
	{
	  // If an assembler name is explicitly specified, there must
	  // be some reason to refer to the symbol from a different
	  // object file.
	  flags |= Backend::function_is_visible;
	}

      // If an inline body refers to this function, then it
      // needs to be visible in the symbol table.
      if (this->is_referenced_by_inline_)
	flags |= Backend::function_is_visible;

      // A go:linkname directive can be used to force a function to be
      // visible.
      if (this->is_exported_by_linkname_)
	flags |= Backend::function_is_visible;

      // If a function calls the predeclared recover function, we
      // can't inline it, because recover behaves differently in a
      // function passed directly to defer.  If this is a recover
      // thunk that we built to test whether a function can be
      // recovered, we can't inline it, because that will mess up
      // our return address comparison.
      bool is_inlinable = !(this->calls_recover_ || this->is_recover_thunk_);

      // If a function calls __go_set_defer_retaddr, then mark it as
      // uninlinable.  This prevents the GCC backend from splitting
      // the function; splitting the function is a bad idea because we
      // want the return address label to be in the same function as
      // the call.
      if (this->calls_defer_retaddr_)
	is_inlinable = false;

      // Check the //go:noinline compiler directive.
      if ((this->pragmas_ & GOPRAGMA_NOINLINE) != 0)
	is_inlinable = false;

      if (is_inlinable)
	flags |= Backend::function_is_inlinable;

      // If this is a thunk created to call a function which calls
      // the predeclared recover function, we need to disable
      // stack splitting for the thunk.
      bool disable_split_stack = this->is_recover_thunk_;

      // Check the //go:nosplit compiler directive.
      if ((this->pragmas_ & GOPRAGMA_NOSPLIT) != 0)
	disable_split_stack = true;

      if (disable_split_stack)
	flags |= Backend::function_no_split_stack;

      // This should go into a unique section if that has been
      // requested elsewhere, or if this is a nointerface function.
      // We want to put a nointerface function into a unique section
      // because there is a good chance that the linker garbage
      // collection can discard it.
      if (this->in_unique_section_
	  || (this->is_method() && this->nointerface()))
	flags |= Backend::function_in_unique_section;

      if (this->is_inline_only_)
	flags |= Backend::function_only_inline;

      Btype* functype = this->type_->get_backend_fntype(gogo);

      Backend_name bname;
      this->backend_name(gogo, no, &bname);

      this->fndecl_ = gogo->backend()->function(functype,
						bname.name(),
						bname.optional_asm_name(),
						flags,
						this->location());
    }
  return this->fndecl_;
}

// Get the backend name.

void
Function_declaration::backend_name(Gogo* gogo, Named_object* no,
				   Backend_name* bname)
{
  if (!this->asm_name_.empty())
    bname->set_asm_name(this->asm_name_);
  else
    {
      Type* rtype = NULL;
      if (this->fntype_->is_method())
	rtype = this->fntype_->receiver()->type();
      gogo->function_backend_name(no->name(), no->package(), rtype, bname);
    }
}

// Get the backend representation.

Bfunction*
Function_declaration::get_or_make_decl(Gogo* gogo, Named_object* no)
{
  if (this->fndecl_ == NULL)
    {
      unsigned int flags =
	(Backend::function_is_visible
	 | Backend::function_is_declaration
	 | Backend::function_is_inlinable);

      // Let Go code use an asm declaration to pick up a builtin
      // function.
      if (!this->asm_name_.empty())
	{
	  Bfunction* builtin_decl =
	    gogo->backend()->lookup_builtin(this->asm_name_);
	  if (builtin_decl != NULL)
	    {
	      this->fndecl_ = builtin_decl;
	      return this->fndecl_;
	    }

	  if (this->asm_name_ == "runtime.gopanic"
	      || this->asm_name_.compare(0, 13, "runtime.panic") == 0
	      || this->asm_name_.compare(0, 15, "runtime.goPanic") == 0
              || this->asm_name_ == "runtime.block")
	    flags |= Backend::function_does_not_return;
	}

      Btype* functype = this->fntype_->get_backend_fntype(gogo);

      Backend_name bname;
      this->backend_name(gogo, no, &bname);

      this->fndecl_ = gogo->backend()->function(functype,
						bname.name(),
						bname.optional_asm_name(),
						flags,
						this->location());
    }

  return this->fndecl_;
}

// Build the descriptor for a function declaration.  This won't
// necessarily happen if the package has just a declaration for the
// function and no other reference to it, but we may still need the
// descriptor for references from other packages.
void
Function_declaration::build_backend_descriptor(Gogo* gogo)
{
  if (this->descriptor_ != NULL)
    {
      Translate_context context(gogo, NULL, NULL, NULL);
      this->descriptor_->get_backend(&context);
    }
}

// Check that the types used in this declaration's signature are defined.
// Reports errors for any undefined type.

void
Function_declaration::check_types() const
{
  // Calling Type::base will give errors for any undefined types.
  Function_type* fntype = this->type();
  if (fntype->receiver() != NULL)
    fntype->receiver()->type()->base();
  if (fntype->parameters() != NULL)
    {
      const Typed_identifier_list* params = fntype->parameters();
      for (Typed_identifier_list::const_iterator p = params->begin();
           p != params->end();
           ++p)
        p->type()->base();
    }
}

// Return the function's decl after it has been built.

Bfunction*
Function::get_decl() const
{
  go_assert(this->fndecl_ != NULL);
  return this->fndecl_;
}

// Build the backend representation for the function code.

void
Function::build(Gogo* gogo, Named_object* named_function)
{
  Translate_context context(gogo, named_function, NULL, NULL);

  // A list of parameter variables for this function.
  std::vector<Bvariable*> param_vars;

  // Variables that need to be declared for this function and their
  // initial values.
  std::vector<Bvariable*> vars;
  std::vector<Expression*> var_inits;
  std::vector<Statement*> var_decls_stmts;
  for (Bindings::const_definitions_iterator p =
	 this->block_->bindings()->begin_definitions();
       p != this->block_->bindings()->end_definitions();
       ++p)
    {
      Location loc = (*p)->location();
      if ((*p)->is_variable() && (*p)->var_value()->is_parameter())
	{
	  Bvariable* bvar = (*p)->get_backend_variable(gogo, named_function);
          Bvariable* parm_bvar = bvar;

	  // We always pass the receiver to a method as a pointer.  If
	  // the receiver is declared as a non-pointer type, then we
	  // copy the value into a local variable.  For direct interface
          // type we pack the pointer into the type.
	  if ((*p)->var_value()->is_receiver()
              && (*p)->var_value()->type()->points_to() == NULL)
	    {
	      std::string name = (*p)->name() + ".pointer";
	      Type* var_type = (*p)->var_value()->type();
	      Variable* parm_var =
		  new Variable(Type::make_pointer_type(var_type), NULL, false,
			       true, false, loc);
	      Named_object* parm_no =
                  Named_object::make_variable(name, NULL, parm_var);
              parm_bvar = parm_no->get_backend_variable(gogo, named_function);

              vars.push_back(bvar);

              Expression* parm_ref =
                  Expression::make_var_reference(parm_no, loc);
              Type* recv_type = (*p)->var_value()->type();
              if (recv_type->is_direct_iface_type())
                parm_ref = Expression::pack_direct_iface(recv_type, parm_ref, loc);
              else
                parm_ref =
                    Expression::make_dereference(parm_ref,
                                                 Expression::NIL_CHECK_NEEDED,
                                                 loc);
              if ((*p)->var_value()->is_in_heap())
                parm_ref = Expression::make_heap_expression(parm_ref, loc);
              var_inits.push_back(parm_ref);
	    }
	  else if ((*p)->var_value()->is_in_heap())
	    {
	      // If we take the address of a parameter, then we need
	      // to copy it into the heap.
	      std::string parm_name = (*p)->name() + ".param";
	      Variable* parm_var = new Variable((*p)->var_value()->type(), NULL,
						false, true, false, loc);
	      Named_object* parm_no =
		  Named_object::make_variable(parm_name, NULL, parm_var);
	      parm_bvar = parm_no->get_backend_variable(gogo, named_function);

              vars.push_back(bvar);
	      Expression* var_ref =
		  Expression::make_var_reference(parm_no, loc);
	      var_ref = Expression::make_heap_expression(var_ref, loc);
              var_inits.push_back(var_ref);
	    }
          param_vars.push_back(parm_bvar);
	}
      else if ((*p)->is_result_variable())
	{
	  Bvariable* bvar = (*p)->get_backend_variable(gogo, named_function);

	  Type* type = (*p)->result_var_value()->type();
	  Expression* init;
	  if (!(*p)->result_var_value()->is_in_heap())
	    {
	      Btype* btype = type->get_backend(gogo);
	      Bexpression* binit = gogo->backend()->zero_expression(btype);
              init = Expression::make_backend(binit, type, loc);
	    }
	  else
	    init = Expression::make_allocation(type, loc);

          vars.push_back(bvar);
          var_inits.push_back(init);
	}
      else if (this->defer_stack_ != NULL
               && (*p)->is_variable()
               && (*p)->var_value()->is_non_escaping_address_taken()
               && !(*p)->var_value()->is_in_heap())
        {
          // Local variable captured by deferred closure needs to be live
          // until the end of the function. We create a top-level
          // declaration for it.
          // TODO: we don't need to do this if the variable is not captured
          // by the defer closure. There is no easy way to check it here,
          // so we do this for all address-taken variables for now.
          Variable* var = (*p)->var_value();
          Temporary_statement* ts =
            Statement::make_temporary(var->type(), NULL, var->location());
          ts->set_is_address_taken();
          var->set_toplevel_decl(ts);
          var_decls_stmts.push_back(ts);
        }
    }
  if (!gogo->backend()->function_set_parameters(this->fndecl_, param_vars))
    {
      go_assert(saw_errors());
      return;
    }

  // If we need a closure variable, make sure to create it.
  // It gets installed in the function as a side effect of creation.
  if (this->closure_var_ != NULL)
    {
      go_assert(this->closure_var_->var_value()->is_closure());
      this->closure_var_->get_backend_variable(gogo, named_function);
    }

  if (this->block_ != NULL)
    {
      // Declare variables if necessary.
      Bblock* var_decls = NULL;
      std::vector<Bstatement*> var_decls_bstmt_list;
      Bstatement* defer_init = NULL;
      if (!vars.empty() || this->defer_stack_ != NULL)
	{
          var_decls =
              gogo->backend()->block(this->fndecl_, NULL, vars,
                                     this->block_->start_location(),
                                     this->block_->end_location());

	  if (this->defer_stack_ != NULL)
	    {
	      Translate_context dcontext(gogo, named_function, this->block_,
                                         var_decls);
              defer_init = this->defer_stack_->get_backend(&dcontext);
              var_decls_bstmt_list.push_back(defer_init);
              for (std::vector<Statement*>::iterator p = var_decls_stmts.begin();
                   p != var_decls_stmts.end();
                   ++p)
                {
                  Bstatement* bstmt = (*p)->get_backend(&dcontext);
                  var_decls_bstmt_list.push_back(bstmt);
                }
	    }
	}

      // Build the backend representation for all the statements in the
      // function.
      Translate_context bcontext(gogo, named_function, NULL, NULL);
      Bblock* code_block = this->block_->get_backend(&bcontext);

      // Initialize variables if necessary.
      Translate_context icontext(gogo, named_function, this->block_,
                                 var_decls);
      std::vector<Bstatement*> init;
      go_assert(vars.size() == var_inits.size());
      for (size_t i = 0; i < vars.size(); ++i)
	{
          Bexpression* binit = var_inits[i]->get_backend(&icontext);
          Bstatement* init_stmt =
              gogo->backend()->init_statement(this->fndecl_, vars[i],
                                              binit);
          init.push_back(init_stmt);
	}
      Bstatement* var_init = gogo->backend()->statement_list(init);

      // Initialize all variables before executing this code block.
      Bstatement* code_stmt = gogo->backend()->block_statement(code_block);
      code_stmt = gogo->backend()->compound_statement(var_init, code_stmt);

      // If we have a defer stack, initialize it at the start of a
      // function.
      Bstatement* except = NULL;
      Bstatement* fini = NULL;
      if (defer_init != NULL)
	{
	  // Clean up the defer stack when we leave the function.
	  this->build_defer_wrapper(gogo, named_function, &except, &fini);

          // Wrap the code for this function in an exception handler to handle
          // defer calls.
          code_stmt =
              gogo->backend()->exception_handler_statement(code_stmt,
                                                           except, fini,
                                                           this->location_);
	}

      // Stick the code into the block we built for the receiver, if
      // we built one.
      if (var_decls != NULL)
        {
          var_decls_bstmt_list.push_back(code_stmt);
          gogo->backend()->block_add_statements(var_decls, var_decls_bstmt_list);
          code_stmt = gogo->backend()->block_statement(var_decls);
        }

      if (!gogo->backend()->function_set_body(this->fndecl_, code_stmt))
        {
          go_assert(saw_errors());
          return;
        }
    }

  // If we created a descriptor for the function, make sure we emit it.
  if (this->descriptor_ != NULL)
    {
      Translate_context dcontext(gogo, NULL, NULL, NULL);
      this->descriptor_->get_backend(&dcontext);
    }
}

// Build the wrappers around function code needed if the function has
// any defer statements.  This sets *EXCEPT to an exception handler
// and *FINI to a finally handler.

void
Function::build_defer_wrapper(Gogo* gogo, Named_object* named_function,
			      Bstatement** except, Bstatement** fini)
{
  Location end_loc = this->block_->end_location();

  // Add an exception handler.  This is used if a panic occurs.  Its
  // purpose is to stop the stack unwinding if a deferred function
  // calls recover.  There are more details in
  // libgo/runtime/go-unwind.c.

  std::vector<Bstatement*> stmts;
  Expression* call = Runtime::make_call(Runtime::CHECKDEFER, end_loc, 1,
					this->defer_stack(end_loc));
  Translate_context context(gogo, named_function, NULL, NULL);
  Bexpression* defer = call->get_backend(&context);
  stmts.push_back(gogo->backend()->expression_statement(this->fndecl_, defer));

  Bstatement* ret_bstmt = this->return_value(gogo, named_function, end_loc);
  if (ret_bstmt != NULL)
    stmts.push_back(ret_bstmt);

  go_assert(*except == NULL);
  *except = gogo->backend()->statement_list(stmts);

  call = Runtime::make_call(Runtime::CHECKDEFER, end_loc, 1,
                            this->defer_stack(end_loc));
  defer = call->get_backend(&context);

  call = Runtime::make_call(Runtime::DEFERRETURN, end_loc, 1,
        		    this->defer_stack(end_loc));
  Bexpression* undefer = call->get_backend(&context);
  Bstatement* function_defer =
      gogo->backend()->function_defer_statement(this->fndecl_, undefer, defer,
                                                end_loc);
  stmts = std::vector<Bstatement*>(1, function_defer);
  if (this->type_->results() != NULL
      && !this->type_->results()->empty()
      && !this->type_->results()->front().name().empty())
    {
      // If the result variables are named, and we are returning from
      // this function rather than panicing through it, we need to
      // return them again, because they might have been changed by a
      // defer function.  The runtime routines set the defer_stack
      // variable to true if we are returning from this function.

      ret_bstmt = this->return_value(gogo, named_function, end_loc);
      Bexpression* nil = Expression::make_nil(end_loc)->get_backend(&context);
      Bexpression* ret =
          gogo->backend()->compound_expression(ret_bstmt, nil, end_loc);
      Expression* ref =
	Expression::make_temporary_reference(this->defer_stack_, end_loc);
      Bexpression* bref = ref->get_backend(&context);
      ret = gogo->backend()->conditional_expression(this->fndecl_,
                                                    NULL, bref, ret, NULL,
                                                    end_loc);
      stmts.push_back(gogo->backend()->expression_statement(this->fndecl_, ret));
    }

  go_assert(*fini == NULL);
  *fini = gogo->backend()->statement_list(stmts);
}

// Return the statement that assigns values to this function's result struct.

Bstatement*
Function::return_value(Gogo* gogo, Named_object* named_function,
		       Location location) const
{
  const Typed_identifier_list* results = this->type_->results();
  if (results == NULL || results->empty())
    return NULL;

  go_assert(this->results_ != NULL);
  if (this->results_->size() != results->size())
    {
      go_assert(saw_errors());
      return gogo->backend()->error_statement();
    }

  std::vector<Bexpression*> vals(results->size());
  for (size_t i = 0; i < vals.size(); ++i)
    {
      Named_object* no = (*this->results_)[i];
      Bvariable* bvar = no->get_backend_variable(gogo, named_function);
      Bexpression* val = gogo->backend()->var_expression(bvar, location);
      if (no->result_var_value()->is_in_heap())
	{
	  Btype* bt = no->result_var_value()->type()->get_backend(gogo);
	  val = gogo->backend()->indirect_expression(bt, val, true, location);
	}
      vals[i] = val;
    }
  return gogo->backend()->return_statement(this->fndecl_, vals, location);
}

// Class Block.

Block::Block(Block* enclosing, Location location)
  : enclosing_(enclosing), statements_(),
    bindings_(new Bindings(enclosing == NULL
			   ? NULL
			   : enclosing->bindings())),
    start_location_(location),
    end_location_(Linemap::unknown_location())
{
}

// Add a statement to a block.

void
Block::add_statement(Statement* statement)
{
  this->statements_.push_back(statement);
}

// Add a statement to the front of a block.  This is slow but is only
// used for reference counts of parameters.

void
Block::add_statement_at_front(Statement* statement)
{
  this->statements_.insert(this->statements_.begin(), statement);
}

// Replace a statement in a block.

void
Block::replace_statement(size_t index, Statement* s)
{
  go_assert(index < this->statements_.size());
  this->statements_[index] = s;
}

// Add a statement before another statement.

void
Block::insert_statement_before(size_t index, Statement* s)
{
  go_assert(index < this->statements_.size());
  this->statements_.insert(this->statements_.begin() + index, s);
}

// Add a statement after another statement.

void
Block::insert_statement_after(size_t index, Statement* s)
{
  go_assert(index < this->statements_.size());
  this->statements_.insert(this->statements_.begin() + index + 1, s);
}

// Traverse the tree.

int
Block::traverse(Traverse* traverse)
{
  unsigned int traverse_mask = traverse->traverse_mask();

  if ((traverse_mask & Traverse::traverse_blocks) != 0)
    {
      int t = traverse->block(this);
      if (t == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
      else if (t == TRAVERSE_SKIP_COMPONENTS)
	return TRAVERSE_CONTINUE;
    }

  if ((traverse_mask
       & (Traverse::traverse_variables
	  | Traverse::traverse_constants
	  | Traverse::traverse_expressions
	  | Traverse::traverse_types)) != 0)
    {
      for (Bindings::const_definitions_iterator pb =
	     this->bindings_->begin_definitions();
	   pb != this->bindings_->end_definitions();
	   ++pb)
	{
	  if ((*pb)->traverse(traverse, false) == TRAVERSE_EXIT)
	    return TRAVERSE_EXIT;
	}
    }

  // No point in checking traverse_mask here--if we got here we always
  // want to walk the statements.  The traversal can insert new
  // statements before or after the current statement.  Inserting
  // statements before the current statement requires updating I via
  // the pointer; those statements will not be traversed.  Any new
  // statements inserted after the current statement will be traversed
  // in their turn.
  for (size_t i = 0; i < this->statements_.size(); ++i)
    {
      if (this->statements_[i]->traverse(this, &i, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }

  return TRAVERSE_CONTINUE;
}

// Work out types for unspecified variables and constants.

void
Block::determine_types()
{
  for (Bindings::const_definitions_iterator pb =
	 this->bindings_->begin_definitions();
       pb != this->bindings_->end_definitions();
       ++pb)
    {
      if ((*pb)->is_variable())
	(*pb)->var_value()->determine_type();
      else if ((*pb)->is_const())
	(*pb)->const_value()->determine_type();
    }

  for (std::vector<Statement*>::const_iterator ps = this->statements_.begin();
       ps != this->statements_.end();
       ++ps)
    (*ps)->determine_types();
}

// Return true if the statements in this block may fall through.

bool
Block::may_fall_through() const
{
  if (this->statements_.empty())
    return true;
  return this->statements_.back()->may_fall_through();
}

// Write export data for a block.

void
Block::export_block(Export_function_body* efb)
{
  for (Block::iterator p = this->begin();
       p != this->end();
       ++p)
    {
      efb->indent();

      efb->increment_indent();
      (*p)->export_statement(efb);
      efb->decrement_indent();

      Location loc = (*p)->location();
      if ((*p)->is_block_statement())
	{
	  // For a block we put the start location on the first brace
	  // in Block_statement::do_export_statement.  Here we put the
	  // end location on the final brace.
	  loc = (*p)->block_statement()->block()->end_location();
	}
      char buf[50];
      snprintf(buf, sizeof buf, " //%d\n", Linemap::location_to_line(loc));
      efb->write_c_string(buf);
    }
}

// Add exported block data to SET, reading from BODY starting at OFF.
// Returns whether the import succeeded.

bool
Block::import_block(Block* set, Import_function_body *ifb, Location loc)
{
  Location eloc = ifb->location();
  Location sloc = loc;
  const std::string& body(ifb->body());
  size_t off = ifb->off();
  while (off < body.length())
    {
      int indent = ifb->indent();
      if (off + indent >= body.length())
	{
	  go_error_at(eloc,
		      "invalid export data for %qs: insufficient indentation",
		      ifb->name().c_str());
	  return false;
	}
      for (int i = 0; i < indent - 1; i++)
	{
	  if (body[off + i] != ' ')
	    {
	      go_error_at(eloc,
			  "invalid export data for %qs: bad indentation",
			  ifb->name().c_str());
	      return false;
	    }
	}

      bool at_end = false;
      if (body[off + indent - 1] == '}')
	at_end = true;
      else if (body[off + indent - 1] != ' ')
	{
	  go_error_at(eloc,
		      "invalid export data for %qs: bad indentation",
		      ifb->name().c_str());
	  return false;
	}

      off += indent;

      size_t nl = body.find('\n', off);
      if (nl == std::string::npos)
	{
	  go_error_at(eloc, "invalid export data for %qs: missing newline",
		      ifb->name().c_str());
	  return false;
	}

      size_t lineno_pos = body.find(" //", off);
      if (lineno_pos == std::string::npos || lineno_pos >= nl)
	{
	  go_error_at(eloc, "invalid export data for %qs: missing line number",
		      ifb->name().c_str());
	  return false;
	}

      unsigned int lineno = 0;
      for (size_t i = lineno_pos + 3; i < nl; ++i)
	{
	  char c = body[i];
	  if (c < '0' || c > '9')
	    {
	      go_error_at(loc,
			  "invalid export data for %qs: invalid line number",
			  ifb->name().c_str());
	      return false;
	    }
	  lineno = lineno * 10 + c - '0';
	}

      ifb->gogo()->linemap()->start_line(lineno, 1);
      sloc = ifb->gogo()->linemap()->get_location(0);

      if (at_end)
	{
	  // An if statement can have an "else" following the "}", in
	  // which case we want to leave the offset where it is, just
	  // after the "}".  We don't get the block ending location
	  // quite right for if statements.
	  if (body.compare(off, 6, " else ") != 0)
	    off = nl + 1;
	  break;
	}

      ifb->set_off(off);
      Statement* s = Statement::import_statement(ifb, sloc);
      if (s == NULL)
	return false;

      set->add_statement(s);

      size_t at = ifb->off();
      if (at < nl + 1)
	off = nl + 1;
      else
	off = at;
    }

  ifb->set_off(off);
  set->set_end_location(sloc);
  return true;
}

// Convert a block to the backend representation.

Bblock*
Block::get_backend(Translate_context* context)
{
  Gogo* gogo = context->gogo();
  Named_object* function = context->function();
  std::vector<Bvariable*> vars;
  vars.reserve(this->bindings_->size_definitions());
  for (Bindings::const_definitions_iterator pv =
	 this->bindings_->begin_definitions();
       pv != this->bindings_->end_definitions();
       ++pv)
    {
      if ((*pv)->is_variable() && !(*pv)->var_value()->is_parameter())
	vars.push_back((*pv)->get_backend_variable(gogo, function));
    }

  go_assert(function != NULL);
  Bfunction* bfunction =
    function->func_value()->get_or_make_decl(gogo, function);
  Bblock* ret = context->backend()->block(bfunction, context->bblock(),
					  vars, this->start_location_,
					  this->end_location_);

  Translate_context subcontext(gogo, function, this, ret);
  std::vector<Bstatement*> bstatements;
  bstatements.reserve(this->statements_.size());
  for (std::vector<Statement*>::const_iterator p = this->statements_.begin();
       p != this->statements_.end();
       ++p)
    bstatements.push_back((*p)->get_backend(&subcontext));

  context->backend()->block_add_statements(ret, bstatements);

  return ret;
}

// Class Bindings_snapshot.

Bindings_snapshot::Bindings_snapshot(const Block* b, Location location)
  : block_(b), counts_(), location_(location)
{
  while (b != NULL)
    {
      this->counts_.push_back(b->bindings()->size_definitions());
      b = b->enclosing();
    }
}

// Report errors appropriate for a goto from B to this.

void
Bindings_snapshot::check_goto_from(const Block* b, Location loc)
{
  size_t dummy;
  if (!this->check_goto_block(loc, b, this->block_, &dummy))
    return;
  this->check_goto_defs(loc, this->block_,
			this->block_->bindings()->size_definitions(),
			this->counts_[0]);
}

// Report errors appropriate for a goto from this to B.

void
Bindings_snapshot::check_goto_to(const Block* b)
{
  size_t index;
  if (!this->check_goto_block(this->location_, this->block_, b, &index))
    return;
  this->check_goto_defs(this->location_, b, this->counts_[index],
			b->bindings()->size_definitions());
}

// Report errors appropriate for a goto at LOC from BFROM to BTO.
// Return true if all is well, false if we reported an error.  If this
// returns true, it sets *PINDEX to the number of blocks BTO is above
// BFROM.

bool
Bindings_snapshot::check_goto_block(Location loc, const Block* bfrom,
				    const Block* bto, size_t* pindex)
{
  // It is an error if BTO is not either BFROM or above BFROM.
  size_t index = 0;
  for (const Block* pb = bfrom; pb != bto; pb = pb->enclosing(), ++index)
    {
      if (pb == NULL)
	{
	  go_error_at(loc, "goto jumps into block");
	  go_inform(bto->start_location(), "goto target block starts here");
	  return false;
	}
    }
  *pindex = index;
  return true;
}

// Report errors appropriate for a goto at LOC ending at BLOCK, where
// CFROM is the number of names defined at the point of the goto and
// CTO is the number of names defined at the point of the label.

void
Bindings_snapshot::check_goto_defs(Location loc, const Block* block,
				   size_t cfrom, size_t cto)
{
  if (cfrom < cto)
    {
      Bindings::const_definitions_iterator p =
	block->bindings()->begin_definitions();
      for (size_t i = 0; i < cfrom; ++i)
	{
	  go_assert(p != block->bindings()->end_definitions());
	  ++p;
	}
      go_assert(p != block->bindings()->end_definitions());

      for (; p != block->bindings()->end_definitions(); ++p)
	{
	  if ((*p)->is_variable())
	    {
	      std::string n = (*p)->message_name();
	      go_error_at(loc, "goto jumps over declaration of %qs", n.c_str());
	      go_inform((*p)->location(), "%qs defined here", n.c_str());
	    }
	}
    }
}

// Class Function_declaration.

// Whether this declares a method.

bool
Function_declaration::is_method() const
{
  return this->fntype_->is_method();
}

// Whether this method should not be included in the type descriptor.

bool
Function_declaration::nointerface() const
{
  go_assert(this->is_method());
  return (this->pragmas_ & GOPRAGMA_NOINTERFACE) != 0;
}

// Record that this method should not be included in the type
// descriptor.

void
Function_declaration::set_nointerface()
{
  this->pragmas_ |= GOPRAGMA_NOINTERFACE;
}

// Set the receiver type.  This is used to remove aliases.

void
Function_declaration::set_receiver_type(Type* rtype)
{
  Function_type* oft = this->fntype_;
  Typed_identifier* rec = new Typed_identifier(oft->receiver()->name(),
					       rtype,
					       oft->receiver()->location());
  Typed_identifier_list* parameters = NULL;
  if (oft->parameters() != NULL)
    parameters = oft->parameters()->copy();
  Typed_identifier_list* results = NULL;
  if (oft->results() != NULL)
    results = oft->results()->copy();
  Function_type* nft = Type::make_function_type(rec, parameters, results,
						oft->location());
  this->fntype_ = nft;
}

// Import an inlinable function.  This is used for an inlinable
// function whose body is recorded in the export data.  Parse the
// export data into a Block and create a regular function using that
// Block as its body.  Redeclare this function declaration as the
// function.

void
Function_declaration::import_function_body(Gogo* gogo, Named_object* no)
{
  go_assert(no->func_declaration_value() == this);
  go_assert(no->package() != NULL);
  const std::string& body(this->imported_body_);
  go_assert(!body.empty());

  // Read the "//FILE:LINE" comment starts the export data.

  size_t indent = 1;
  if (this->is_method())
    indent = 2;
  size_t i = 0;
  for (; i < indent; i++)
    {
      if (body.at(i) != ' ')
	{
	  go_error_at(this->location_,
		      "invalid export body for %qs: bad initial indentation",
		      no->message_name().c_str());
	  return;
	}
    }

  if (body.substr(i, 2) != "//")
    {
      go_error_at(this->location_,
		  "invalid export body for %qs: missing file comment",
		  no->message_name().c_str());
      return;
    }

  size_t colon = body.find(':', i + 2);
  size_t nl = body.find('\n', i + 2);
  if (nl == std::string::npos)
    {
      go_error_at(this->location_,
		  "invalid export body for %qs: missing file name",
		  no->message_name().c_str());
      return;
    }
  if (colon == std::string::npos || nl < colon)
    {
      go_error_at(this->location_,
		  "invalid export body for %qs: missing initial line number",
		  no->message_name().c_str());
      return;
    }

  std::string file = body.substr(i + 2, colon - (i + 2));
  std::string linestr = body.substr(colon + 1, nl - (colon + 1));
  char* end;
  long linenol = strtol(linestr.c_str(), &end, 10);
  if (*end != '\0')
    {
      go_error_at(this->location_,
		  "invalid export body for %qs: invalid initial line number",
		  no->message_name().c_str());
      return;
    }
  unsigned int lineno = static_cast<unsigned int>(linenol);

  // Turn the file/line into a location.

  char* alc = new char[file.length() + 1];
  memcpy(alc, file.data(), file.length());
  alc[file.length()] = '\0';
  gogo->linemap()->start_file(alc, lineno);
  gogo->linemap()->start_line(lineno, 1);
  Location start_loc = gogo->linemap()->get_location(0);

  // Define the function with an outer block that declares the
  // parameters.

  Function_type* fntype = this->fntype_;

  Block* outer = new Block(NULL, start_loc);

  Function* fn = new Function(fntype, NULL, outer, start_loc);
  fn->set_is_inline_only();

  if (fntype->is_method())
    {
      if (this->nointerface())
        fn->set_nointerface();
      const Typed_identifier* receiver = fntype->receiver();
      Variable* recv_param = new Variable(receiver->type(), NULL, false,
					  true, true, start_loc);

      std::string rname = receiver->name();
      unsigned rcounter = 0;

      // We need to give a nameless receiver a name to avoid having it
      // clash with some other nameless param. FIXME.
      Gogo::rename_if_empty(&rname, "r", &rcounter);

      outer->bindings()->add_variable(rname, NULL, recv_param);
    }

  const Typed_identifier_list* params = fntype->parameters();
  bool is_varargs = fntype->is_varargs();
  unsigned pcounter = 0;
  if (params != NULL)
    {
      for (Typed_identifier_list::const_iterator p = params->begin();
	   p != params->end();
	   ++p)
	{
	  Variable* param = new Variable(p->type(), NULL, false, true, false,
					 start_loc);
	  if (is_varargs && p + 1 == params->end())
	    param->set_is_varargs_parameter();

	  std::string pname = p->name();

          // We need to give each nameless parameter a non-empty name to avoid
          // having it clash with some other nameless param. FIXME.
          Gogo::rename_if_empty(&pname, "p", &pcounter);

	  outer->bindings()->add_variable(pname, NULL, param);
	}
    }

  fn->create_result_variables(gogo);

  if (!fntype->is_method())
    {
      const Package* package = no->package();
      no = package->bindings()->add_function(no->name(), package, fn);
    }
  else
    {
      Named_type* rtype = fntype->receiver()->type()->deref()->named_type();
      go_assert(rtype != NULL);
      no = rtype->add_method(no->name(), fn);
      const Package* package = rtype->named_object()->package();
      package->bindings()->add_method(no);
    }

  Import_function_body ifb(gogo, this->imp_, no, body, nl + 1, outer, indent);

  if (!Block::import_block(outer, &ifb, start_loc))
    return;

  gogo->lower_block(no, outer);
  outer->determine_types();

  gogo->add_imported_inline_function(no);
}

// Return the function descriptor.

Expression*
Function_declaration::descriptor(Gogo*, Named_object* no)
{
  go_assert(!this->fntype_->is_method());
  if (this->descriptor_ == NULL)
    this->descriptor_ = Expression::make_func_descriptor(no);
  return this->descriptor_;
}

// Class Variable.

Variable::Variable(Type* type, Expression* init, bool is_global,
		   bool is_parameter, bool is_receiver,
		   Location location)
  : type_(type), init_(init), preinit_(NULL), location_(location),
    toplevel_decl_(NULL), init_refs_(NULL), embeds_(NULL), backend_(NULL),
    is_global_(is_global), is_parameter_(is_parameter), is_closure_(false),
    is_receiver_(is_receiver), is_varargs_parameter_(false),
    is_global_sink_(false), is_used_(false), is_address_taken_(false),
    is_non_escaping_address_taken_(false), seen_(false),
    init_is_lowered_(false), init_is_flattened_(false),
    type_from_init_tuple_(false), type_from_range_index_(false),
    type_from_range_value_(false), type_from_chan_element_(false),
    is_type_switch_var_(false), determined_type_(false),
    in_unique_section_(false), is_referenced_by_inline_(false)
{
  go_assert(type != NULL || init != NULL);
  go_assert(!is_parameter || init == NULL);
}

// Traverse the initializer expression.

int
Variable::traverse_expression(Traverse* traverse, unsigned int traverse_mask)
{
  if (this->preinit_ != NULL)
    {
      if (this->preinit_->traverse(traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  if (this->init_ != NULL
      && ((traverse_mask
	   & (Traverse::traverse_expressions | Traverse::traverse_types))
	  != 0))
    {
      if (Expression::traverse(&this->init_, traverse) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }
  return TRAVERSE_CONTINUE;
}

// Lower the initialization expression after parsing is complete.

void
Variable::lower_init_expression(Gogo* gogo, Named_object* function,
				Statement_inserter* inserter)
{
  Named_object* dep = gogo->var_depends_on(this);
  if (dep != NULL && dep->is_variable())
    dep->var_value()->lower_init_expression(gogo, function, inserter);

  if (this->embeds_ != NULL)
    {
      // Now that we have seen any possible type aliases, convert the
      // go:embed directives into an initializer.
      go_assert(this->init_ == NULL && this->type_ != NULL);
      this->init_ = gogo->initializer_for_embeds(this->type_, this->embeds_,
						 this->location_);
      delete this->embeds_;
      this->embeds_ = NULL;
    }

  if (this->init_ != NULL && !this->init_is_lowered_)
    {
      if (this->seen_)
	{
	  // We will give an error elsewhere, this is just to prevent
	  // an infinite loop.
	  return;
	}
      this->seen_ = true;

      Statement_inserter global_inserter;
      if (this->is_global_)
	{
	  global_inserter = Statement_inserter(gogo, this);
	  inserter = &global_inserter;
	}

      gogo->lower_expression(function, inserter, &this->init_);

      this->seen_ = false;

      this->init_is_lowered_ = true;
    }
}

// Flatten the initialization expression after ordering evaluations.

void
Variable::flatten_init_expression(Gogo* gogo, Named_object* function,
                                  Statement_inserter* inserter)
{
  Named_object* dep = gogo->var_depends_on(this);
  if (dep != NULL && dep->is_variable())
    dep->var_value()->flatten_init_expression(gogo, function, inserter);

  if (this->init_ != NULL && !this->init_is_flattened_)
    {
      if (this->seen_)
	{
	  // We will give an error elsewhere, this is just to prevent
	  // an infinite loop.
	  return;
	}
      this->seen_ = true;

      Statement_inserter global_inserter;
      if (this->is_global_)
	{
	  global_inserter = Statement_inserter(gogo, this);
	  inserter = &global_inserter;
	}

      gogo->flatten_expression(function, inserter, &this->init_);

      // If an interface conversion is needed, we need a temporary
      // variable.
      if (this->type_ != NULL
	  && !Type::are_identical(this->type_, this->init_->type(),
				  Type::COMPARE_ERRORS | Type::COMPARE_TAGS,
				  NULL)
	  && this->init_->type()->interface_type() != NULL
	  && !this->init_->is_multi_eval_safe())
	{
	  Temporary_statement* temp =
	    Statement::make_temporary(NULL, this->init_, this->location_);
	  inserter->insert(temp);
	  this->init_ = Expression::make_temporary_reference(temp,
							     this->location_);
	}

      this->seen_ = false;
      this->init_is_flattened_ = true;
    }
}

// Get the preinit block.

Block*
Variable::preinit_block(Gogo* gogo)
{
  go_assert(this->is_global_);
  if (this->preinit_ == NULL)
    this->preinit_ = new Block(NULL, this->location());

  // If a global variable has a preinitialization statement, then we
  // need to have an initialization function.
  gogo->set_need_init_fn();

  return this->preinit_;
}

// Add a statement to be run before the initialization expression.

void
Variable::add_preinit_statement(Gogo* gogo, Statement* s)
{
  Block* b = this->preinit_block(gogo);
  b->add_statement(s);
  b->set_end_location(s->location());
}

// Whether this variable has a type.

bool
Variable::has_type() const
{
  if (this->type_ == NULL)
    return false;

  // A variable created in a type switch case nil does not actually
  // have a type yet.  It will be changed to use the initializer's
  // type in determine_type.
  if (this->is_type_switch_var_
      && this->type_->is_nil_constant_as_type())
    return false;

  return true;
}

// In an assignment which sets a variable to a tuple of EXPR, return
// the type of the first element of the tuple.

Type*
Variable::type_from_tuple(Expression* expr, bool report_error) const
{
  if (expr->map_index_expression() != NULL)
    {
      Map_type* mt = expr->map_index_expression()->get_map_type();
      if (mt == NULL)
	return Type::make_error_type();
      return mt->val_type();
    }
  else if (expr->receive_expression() != NULL)
    {
      Expression* channel = expr->receive_expression()->channel();
      Type* channel_type = channel->type();
      if (channel_type->channel_type() == NULL)
	return Type::make_error_type();
      return channel_type->channel_type()->element_type();
    }
  else
    {
      if (report_error)
	go_error_at(this->location(), "invalid tuple definition");
      return Type::make_error_type();
    }
}

// Given EXPR used in a range clause, return either the index type or
// the value type of the range, depending upon GET_INDEX_TYPE.

Type*
Variable::type_from_range(Expression* expr, bool get_index_type,
			  bool report_error) const
{
  Type* t = expr->type();
  if (t->array_type() != NULL
      || (t->points_to() != NULL
	  && t->points_to()->array_type() != NULL
	  && !t->points_to()->is_slice_type()))
    {
      if (get_index_type)
	return Type::lookup_integer_type("int");
      else
	return t->deref()->array_type()->element_type();
    }
  else if (t->is_string_type())
    {
      if (get_index_type)
	return Type::lookup_integer_type("int");
      else
	return Type::lookup_integer_type("int32");
    }
  else if (t->map_type() != NULL)
    {
      if (get_index_type)
	return t->map_type()->key_type();
      else
	return t->map_type()->val_type();
    }
  else if (t->channel_type() != NULL)
    {
      if (get_index_type)
	return t->channel_type()->element_type();
      else
	{
	  if (report_error)
	    go_error_at(this->location(),
			("invalid definition of value variable "
			 "for channel range"));
	  return Type::make_error_type();
	}
    }
  else
    {
      if (report_error)
	go_error_at(this->location(), "invalid type for range clause");
      return Type::make_error_type();
    }
}

// EXPR should be a channel.  Return the channel's element type.

Type*
Variable::type_from_chan_element(Expression* expr, bool report_error) const
{
  Type* t = expr->type();
  if (t->channel_type() != NULL)
    return t->channel_type()->element_type();
  else
    {
      if (report_error)
	go_error_at(this->location(), "expected channel");
      return Type::make_error_type();
    }
}

// Return the type of the Variable.  This may be called before
// Variable::determine_type is called, which means that we may need to
// get the type from the initializer.  FIXME: If we combine lowering
// with type determination, then this should be unnecessary.

Type*
Variable::type()
{
  // A variable in a type switch with a nil case will have the wrong
  // type here.  This gets fixed up in determine_type, below.
  Type* type = this->type_;
  Expression* init = this->init_;
  if (this->is_type_switch_var_
      && type != NULL
      && this->type_->is_nil_constant_as_type())
    {
      Type_guard_expression* tge = this->init_->type_guard_expression();
      go_assert(tge != NULL);
      init = tge->expr();
      type = NULL;
    }

  if (this->seen_)
    {
      if (this->type_ == NULL || !this->type_->is_error_type())
	{
	  go_error_at(this->location_, "variable initializer refers to itself");
	  this->type_ = Type::make_error_type();
	}
      return this->type_;
    }

  this->seen_ = true;

  if (type != NULL)
    ;
  else if (this->type_from_init_tuple_)
    type = this->type_from_tuple(init, false);
  else if (this->type_from_range_index_ || this->type_from_range_value_)
    type = this->type_from_range(init, this->type_from_range_index_, false);
  else if (this->type_from_chan_element_)
    type = this->type_from_chan_element(init, false);
  else
    {
      go_assert(init != NULL);
      type = init->type();
      go_assert(type != NULL);

      // Variables should not have abstract types.
      if (type->is_abstract())
	type = type->make_non_abstract_type();

      if (type->is_void_type())
	type = Type::make_error_type();
    }

  this->seen_ = false;

  return type;
}

// Fetch the type from a const pointer, in which case it should have
// been set already.

Type*
Variable::type() const
{
  go_assert(this->type_ != NULL);
  return this->type_;
}

// Set the type if necessary.

void
Variable::determine_type()
{
  if (this->determined_type_)
    return;
  this->determined_type_ = true;

  if (this->preinit_ != NULL)
    this->preinit_->determine_types();

  // A variable in a type switch with a nil case will have the wrong
  // type here.  It will have an initializer which is a type guard.
  // We want to initialize it to the value without the type guard, and
  // use the type of that value as well.
  if (this->is_type_switch_var_
      && this->type_ != NULL
      && this->type_->is_nil_constant_as_type())
    {
      Type_guard_expression* tge = this->init_->type_guard_expression();
      go_assert(tge != NULL);
      this->type_ = NULL;
      this->init_ = tge->expr();
    }

  if (this->init_ == NULL)
    go_assert(this->type_ != NULL && !this->type_->is_abstract());
  else if (this->type_from_init_tuple_)
    {
      Expression *init = this->init_;
      init->determine_type_no_context();
      this->type_ = this->type_from_tuple(init, true);
      this->init_ = NULL;
    }
  else if (this->type_from_range_index_ || this->type_from_range_value_)
    {
      Expression* init = this->init_;
      init->determine_type_no_context();
      this->type_ = this->type_from_range(init, this->type_from_range_index_,
					  true);
      this->init_ = NULL;
    }
  else if (this->type_from_chan_element_)
    {
      Expression* init = this->init_;
      init->determine_type_no_context();
      this->type_ = this->type_from_chan_element(init, true);
      this->init_ = NULL;
    }
  else
    {
      Type_context context(this->type_, false);
      this->init_->determine_type(&context);
      if (this->type_ == NULL)
	{
	  Type* type = this->init_->type();
	  go_assert(type != NULL);
	  if (type->is_abstract())
	    type = type->make_non_abstract_type();

	  if (type->is_void_type())
	    {
	      go_error_at(this->location_, "variable has no type");
	      type = Type::make_error_type();
	    }
	  else if (type->is_nil_type())
	    {
	      go_error_at(this->location_, "variable defined to nil type");
	      type = Type::make_error_type();
	    }
	  else if (type->is_call_multiple_result_type())
	    {
	      go_error_at(this->location_,
		       "single variable set to multiple-value function call");
	      type = Type::make_error_type();
	    }

	  this->type_ = type;
	}
    }
}

// Get the initial value of a variable.  This does not
// consider whether the variable is in the heap--it returns the
// initial value as though it were always stored in the stack.

Bexpression*
Variable::get_init(Gogo* gogo, Named_object* function)
{
  go_assert(this->preinit_ == NULL);
  Location loc = this->location();
  if (this->init_ == NULL)
    {
      go_assert(!this->is_parameter_);
      if (this->is_global_ || this->is_in_heap())
	return NULL;
      Btype* btype = this->type()->get_backend(gogo);
      return gogo->backend()->zero_expression(btype);
    }
  else
    {
      Translate_context context(gogo, function, NULL, NULL);
      Expression* init = Expression::make_cast(this->type(), this->init_, loc);
      return init->get_backend(&context);
    }
}

// Get the initial value of a variable when a block is required.
// VAR_DECL is the decl to set; it may be NULL for a sink variable.

Bstatement*
Variable::get_init_block(Gogo* gogo, Named_object* function,
                         Bvariable* var_decl)
{
  go_assert(this->preinit_ != NULL);

  // We want to add the variable assignment to the end of the preinit
  // block.

  Translate_context context(gogo, function, NULL, NULL);
  Bblock* bblock = this->preinit_->get_backend(&context);
  Bfunction* bfunction =
      function->func_value()->get_or_make_decl(gogo, function);

  // It's possible to have pre-init statements without an initializer
  // if the pre-init statements set the variable.
  Bstatement* decl_init = NULL;
  if (this->init_ != NULL)
    {
      if (var_decl == NULL)
        {
          Bexpression* init_bexpr = this->init_->get_backend(&context);
          decl_init = gogo->backend()->expression_statement(bfunction,
                                                            init_bexpr);
        }
      else
	{
          Location loc = this->location();
          Expression* val_expr =
              Expression::make_cast(this->type(), this->init_, loc);
          Bexpression* val = val_expr->get_backend(&context);
          Bexpression* var_ref =
              gogo->backend()->var_expression(var_decl, loc);
          decl_init = gogo->backend()->assignment_statement(bfunction, var_ref,
                                                            val, loc);
	}
    }
  Bstatement* block_stmt = gogo->backend()->block_statement(bblock);
  if (decl_init != NULL)
    block_stmt = gogo->backend()->compound_statement(block_stmt, decl_init);
  return block_stmt;
}

// Add an initializer reference.

void
Variable::add_init_ref(Named_object* var)
{
  if (this->init_refs_ == NULL)
    this->init_refs_ = new std::vector<Named_object*>;
  this->init_refs_->push_back(var);
}

// Export the variable

void
Variable::export_var(Export* exp, const Named_object* no) const
{
  go_assert(this->is_global_);
  exp->write_c_string("var ");
  if (no->package() != NULL)
    {
      char buf[50];
      snprintf(buf, sizeof buf, "<p%d>", exp->package_index(no->package()));
      exp->write_c_string(buf);
    }

  if (!Gogo::is_hidden_name(no->name()))
    exp->write_string(no->name());
  else
    {
      exp->write_c_string(".");
      exp->write_string(Gogo::unpack_hidden_name(no->name()));
    }

  exp->write_c_string(" ");
  exp->write_type(this->type());
  exp->write_c_string("\n");
}

// Import a variable.

bool
Variable::import_var(Import* imp, std::string* pname, Package** ppkg,
		     bool* pis_exported, Type** ptype)
{
  imp->require_c_string("var ");
  if (!Import::read_qualified_identifier(imp, pname, ppkg, pis_exported))
    {
      go_error_at(imp->location(),
		  "import error at %d: bad variable name in export data",
		  imp->pos());
      return false;
    }
  imp->require_c_string(" ");
  *ptype = imp->read_type();
  imp->require_semicolon_if_old_version();
  imp->require_c_string("\n");
  return true;
}

// Convert a variable to the backend representation.

Bvariable*
Variable::get_backend_variable(Gogo* gogo, Named_object* function,
			       const Package* package, const std::string& name)
{
  if (this->backend_ == NULL)
    {
      Backend* backend = gogo->backend();
      Type* type = this->type_;
      if (type->is_error_type()
	  || (type->is_undefined()
	      && (!this->is_global_ || package == NULL)))
	this->backend_ = backend->error_variable();
      else
	{
	  bool is_parameter = this->is_parameter_;
	  if (this->is_receiver_ && type->points_to() == NULL)
	    is_parameter = false;
	  if (this->is_in_heap())
	    {
	      is_parameter = false;
	      type = Type::make_pointer_type(type);
	    }

	  Btype* btype = type->get_backend(gogo);

	  Bvariable* bvar;
	  if (Map_type::is_zero_value(this))
	    bvar = Map_type::backend_zero_value(gogo);
	  else if (this->is_global_)
	    {
	      Backend_name bname;
	      gogo->global_var_backend_name(name, package, &bname);

	      bool is_hidden = Gogo::is_hidden_name(name);
	      // Hack to export runtime.writeBarrier.  FIXME.
	      // This is because go:linkname doesn't work on variables.
	      if (gogo->compiling_runtime()
		  && bname.name() == "runtime.writeBarrier")
		is_hidden = false;

	      // If an inline body refers to this variable, then it
	      // needs to be visible in the symbol table.
	      if (this->is_referenced_by_inline_)
		is_hidden = false;

	      // If this variable is in a different package, then it
	      // can't be treated as a hidden symbol.  This case can
	      // arise when an inlined function refers to a
	      // package-scope unexported variable.
	      if (package != NULL)
		is_hidden = false;

	      unsigned int flags = 0;
	      if (this->is_address_taken_
		  || this->is_non_escaping_address_taken_)
		flags |= Backend::variable_address_is_taken;
	      if (package != NULL)
		flags |= Backend::variable_is_external;
	      if (is_hidden)
		flags |= Backend::variable_is_hidden;
	      if (this->in_unique_section_)
		flags |= Backend::variable_in_unique_section;

	      // For some reason asm_name can't be the empty string
	      // for global_variable, so we call asm_name rather than
	      // optional_asm_name here.  FIXME.

	      bvar = backend->global_variable(bname.name(),
					      bname.asm_name(),
					      btype, flags,
					      this->location_);
	    }
	  else if (function == NULL)
	    {
	      go_assert(saw_errors());
	      bvar = backend->error_variable();
	    }
	  else
	    {
	      const std::string n = Gogo::unpack_hidden_name(name);
	      Bfunction* bfunction = function->func_value()->get_decl();
	      unsigned int flags = 0;
	      if (this->is_non_escaping_address_taken_
		  && !this->is_in_heap())
		flags |= Backend::variable_address_is_taken;
	      if (this->is_closure())
		bvar = backend->static_chain_variable(bfunction, n, btype,
						      flags, this->location_);
	      else if (is_parameter)
		bvar = backend->parameter_variable(bfunction, n, btype,
						   flags, this->location_);
	      else
                {
                  Bvariable* bvar_decl = NULL;
                  if (this->toplevel_decl_ != NULL)
                    {
                      Translate_context context(gogo, NULL, NULL, NULL);
                      bvar_decl = this->toplevel_decl_->temporary_statement()
                        ->get_backend_variable(&context);
                    }
                  bvar = backend->local_variable(bfunction, n, btype,
                                                 bvar_decl, flags,
                                                 this->location_);
                }
	    }
	  this->backend_ = bvar;
	}
    }
  return this->backend_;
}

// Class Result_variable.

// Convert a result variable to the backend representation.

Bvariable*
Result_variable::get_backend_variable(Gogo* gogo, Named_object* function,
				      const std::string& name)
{
  if (this->backend_ == NULL)
    {
      Backend* backend = gogo->backend();
      Type* type = this->type_;
      if (type->is_error())
	this->backend_ = backend->error_variable();
      else
	{
	  if (this->is_in_heap())
	    type = Type::make_pointer_type(type);
	  Btype* btype = type->get_backend(gogo);
	  Bfunction* bfunction = function->func_value()->get_decl();
	  std::string n = Gogo::unpack_hidden_name(name);
	  unsigned int flags = 0;
	  if (this->is_non_escaping_address_taken_
	      && !this->is_in_heap())
	    flags |= Backend::variable_address_is_taken;
	  this->backend_ = backend->local_variable(bfunction, n, btype,
						   NULL, flags,
						   this->location_);
	}
    }
  return this->backend_;
}

// Class Named_constant.

// Set the type of a named constant.  This is only used to set the
// type to an error type.

void
Named_constant::set_type(Type* t)
{
  go_assert(this->type_ == NULL || t->is_error_type());
  this->type_ = t;
}

// Traverse the initializer expression.

int
Named_constant::traverse_expression(Traverse* traverse)
{
  return Expression::traverse(&this->expr_, traverse);
}

// Determine the type of the constant.

void
Named_constant::determine_type()
{
  if (this->type_ != NULL)
    {
      Type_context context(this->type_, false);
      this->expr_->determine_type(&context);
    }
  else
    {
      // A constant may have an abstract type.
      Type_context context(NULL, true);
      this->expr_->determine_type(&context);
      this->type_ = this->expr_->type();
      go_assert(this->type_ != NULL);
    }
}

// Indicate that we found and reported an error for this constant.

void
Named_constant::set_error()
{
  this->type_ = Type::make_error_type();
  this->expr_ = Expression::make_error(this->location_);
}

// Export a constant.

void
Named_constant::export_const(Export* exp, const std::string& name) const
{
  exp->write_c_string("const ");
  exp->write_string(name);
  exp->write_c_string(" ");
  if (!this->type_->is_abstract())
    {
      exp->write_type(this->type_);
      exp->write_c_string(" ");
    }
  exp->write_c_string("= ");

  Export_function_body efb(exp, 0);
  if (!this->type_->is_abstract())
    efb.set_type_context(this->type_);
  this->expr()->export_expression(&efb);
  exp->write_string(efb.body());

  exp->write_c_string("\n");
}

// Import a constant.

void
Named_constant::import_const(Import* imp, std::string* pname, Type** ptype,
			     Expression** pexpr)
{
  imp->require_c_string("const ");
  *pname = imp->read_identifier();
  imp->require_c_string(" ");
  if (imp->peek_char() == '=')
    *ptype = NULL;
  else
    {
      *ptype = imp->read_type();
      imp->require_c_string(" ");
    }
  imp->require_c_string("= ");
  *pexpr = Expression::import_expression(imp, imp->location());
  imp->require_semicolon_if_old_version();
  imp->require_c_string("\n");
}

// Get the backend representation.

Bexpression*
Named_constant::get_backend(Gogo* gogo, Named_object* const_no)
{
  if (this->bconst_ == NULL)
    {
      Translate_context subcontext(gogo, NULL, NULL, NULL);
      Type* type = this->type();
      Location loc = this->location();

      Expression* const_ref = Expression::make_const_reference(const_no, loc);
      Bexpression* const_decl = const_ref->get_backend(&subcontext);
      if (type != NULL && type->is_numeric_type())
	{
	  Btype* btype = type->get_backend(gogo);
	  std::string name;
	  if (const_no->package() == NULL)
	    name = gogo->pkgpath();
	  else
	    name = const_no->package()->pkgpath();
	  name.push_back('.');
	  name.append(Gogo::unpack_hidden_name(const_no->name()));
	  const_decl =
	    gogo->backend()->named_constant_expression(btype, name,
						       const_decl, loc);
	}
      this->bconst_ = const_decl;
    }
  return this->bconst_;
}

// Add a method.

Named_object*
Type_declaration::add_method(const std::string& name, Function* function)
{
  Named_object* ret = Named_object::make_function(name, NULL, function);
  this->methods_.push_back(ret);
  return ret;
}

// Add a method declaration.

Named_object*
Type_declaration::add_method_declaration(const std::string&  name,
					 Package* package,
					 Function_type* type,
					 Location location)
{
  Named_object* ret = Named_object::make_function_declaration(name, package,
							      type, location);
  this->methods_.push_back(ret);
  return ret;
}

// Return whether any methods are defined.

bool
Type_declaration::has_methods() const
{
  return !this->methods_.empty();
}

// Define methods for the real type.

void
Type_declaration::define_methods(Named_type* nt)
{
  if (this->methods_.empty())
    return;

  while (nt->is_alias())
    {
      Type *t = nt->real_type()->forwarded();
      if (t->named_type() != NULL)
	nt = t->named_type();
      else if (t->forward_declaration_type() != NULL)
	{
	  Named_object* no = t->forward_declaration_type()->named_object();
	  Type_declaration* td = no->type_declaration_value();
	  td->methods_.insert(td->methods_.end(), this->methods_.begin(),
			      this->methods_.end());
	  this->methods_.clear();
	  return;
	}
      else
	{
	  for (std::vector<Named_object*>::const_iterator p =
		 this->methods_.begin();
	       p != this->methods_.end();
	       ++p)
	    go_error_at((*p)->location(),
			("invalid receiver type "
			 "(receiver must be a named type)"));
	  return;
	}
    }

  for (std::vector<Named_object*>::const_iterator p = this->methods_.begin();
       p != this->methods_.end();
       ++p)
    {
      if ((*p)->is_function_declaration()
	  || !(*p)->func_value()->is_sink())
	nt->add_existing_method(*p);
    }
}

// We are using the type.  Return true if we should issue a warning.

bool
Type_declaration::using_type()
{
  bool ret = !this->issued_warning_;
  this->issued_warning_ = true;
  return ret;
}

// Class Unknown_name.

// Set the real named object.

void
Unknown_name::set_real_named_object(Named_object* no)
{
  go_assert(this->real_named_object_ == NULL);
  go_assert(!no->is_unknown());
  this->real_named_object_ = no;
}

// Class Named_object.

Named_object::Named_object(const std::string& name,
			   const Package* package,
			   Classification classification)
  : name_(name), package_(package), classification_(classification),
    is_redefinition_(false)
{
  if (Gogo::is_sink_name(name))
    go_assert(classification == NAMED_OBJECT_SINK);
}

// Make an unknown name.  This is used by the parser.  The name must
// be resolved later.  Unknown names are only added in the current
// package.

Named_object*
Named_object::make_unknown_name(const std::string& name,
				Location location)
{
  Named_object* named_object = new Named_object(name, NULL,
						NAMED_OBJECT_UNKNOWN);
  Unknown_name* value = new Unknown_name(location);
  named_object->u_.unknown_value = value;
  return named_object;
}

// Make a constant.

Named_object*
Named_object::make_constant(const Typed_identifier& tid,
			    const Package* package, Expression* expr,
			    int iota_value)
{
  Named_object* named_object = new Named_object(tid.name(), package,
						NAMED_OBJECT_CONST);
  Named_constant* named_constant = new Named_constant(tid.type(), expr,
						      iota_value,
						      tid.location());
  named_object->u_.const_value = named_constant;
  return named_object;
}

// Make a named type.

Named_object*
Named_object::make_type(const std::string& name, const Package* package,
			Type* type, Location location)
{
  Named_object* named_object = new Named_object(name, package,
						NAMED_OBJECT_TYPE);
  Named_type* named_type = Type::make_named_type(named_object, type, location);
  named_object->u_.type_value = named_type;
  return named_object;
}

// Make a type declaration.

Named_object*
Named_object::make_type_declaration(const std::string& name,
				    const Package* package,
				    Location location)
{
  Named_object* named_object = new Named_object(name, package,
						NAMED_OBJECT_TYPE_DECLARATION);
  Type_declaration* type_declaration = new Type_declaration(location);
  named_object->u_.type_declaration = type_declaration;
  return named_object;
}

// Make a variable.

Named_object*
Named_object::make_variable(const std::string& name, const Package* package,
			    Variable* variable)
{
  Named_object* named_object = new Named_object(name, package,
						NAMED_OBJECT_VAR);
  named_object->u_.var_value = variable;
  return named_object;
}

// Make a result variable.

Named_object*
Named_object::make_result_variable(const std::string& name,
				   Result_variable* result)
{
  Named_object* named_object = new Named_object(name, NULL,
						NAMED_OBJECT_RESULT_VAR);
  named_object->u_.result_var_value = result;
  return named_object;
}

// Make a sink.  This is used for the special blank identifier _.

Named_object*
Named_object::make_sink()
{
  return new Named_object("_", NULL, NAMED_OBJECT_SINK);
}

// Make a named function.

Named_object*
Named_object::make_function(const std::string& name, const Package* package,
			    Function* function)
{
  Named_object* named_object = new Named_object(name, package,
						NAMED_OBJECT_FUNC);
  named_object->u_.func_value = function;
  return named_object;
}

// Make a function declaration.

Named_object*
Named_object::make_function_declaration(const std::string& name,
					const Package* package,
					Function_type* fntype,
					Location location)
{
  Named_object* named_object = new Named_object(name, package,
						NAMED_OBJECT_FUNC_DECLARATION);
  Function_declaration *func_decl = new Function_declaration(fntype, location);
  named_object->u_.func_declaration_value = func_decl;
  return named_object;
}

// Make a package.

Named_object*
Named_object::make_package(const std::string& alias, Package* package)
{
  Named_object* named_object = new Named_object(alias, NULL,
						NAMED_OBJECT_PACKAGE);
  named_object->u_.package_value = package;
  return named_object;
}

// Return the name to use in an error message.

std::string
Named_object::message_name() const
{
  if (this->package_ == NULL)
    return Gogo::message_name(this->name_);
  std::string ret;
  if (this->package_->has_package_name())
    ret = this->package_->package_name();
  else
    ret = this->package_->pkgpath();
  ret = Gogo::message_name(ret);
  ret += '.';
  ret += Gogo::message_name(this->name_);
  return ret;
}

// Set the type when a declaration is defined.

void
Named_object::set_type_value(Named_type* named_type)
{
  go_assert(this->classification_ == NAMED_OBJECT_TYPE_DECLARATION);
  Type_declaration* td = this->u_.type_declaration;
  td->define_methods(named_type);
  unsigned int index;
  Named_object* in_function = td->in_function(&index);
  if (in_function != NULL)
    named_type->set_in_function(in_function, index);
  delete td;
  this->classification_ = NAMED_OBJECT_TYPE;
  this->u_.type_value = named_type;
}

// Define a function which was previously declared.

void
Named_object::set_function_value(Function* function)
{
  go_assert(this->classification_ == NAMED_OBJECT_FUNC_DECLARATION);
  if (this->func_declaration_value()->has_descriptor())
    {
      Expression* descriptor =
	this->func_declaration_value()->descriptor(NULL, NULL);
      function->set_descriptor(descriptor);
    }
  this->classification_ = NAMED_OBJECT_FUNC;
  // FIXME: We should free the old value.
  this->u_.func_value = function;
}

// Declare an unknown object as a type declaration.

void
Named_object::declare_as_type()
{
  go_assert(this->classification_ == NAMED_OBJECT_UNKNOWN);
  Unknown_name* unk = this->u_.unknown_value;
  this->classification_ = NAMED_OBJECT_TYPE_DECLARATION;
  this->u_.type_declaration = new Type_declaration(unk->location());
  delete unk;
}

// Return the location of a named object.

Location
Named_object::location() const
{
  switch (this->classification_)
    {
    default:
    case NAMED_OBJECT_UNINITIALIZED:
      go_unreachable();

    case NAMED_OBJECT_ERRONEOUS:
      return Linemap::unknown_location();

    case NAMED_OBJECT_UNKNOWN:
      return this->unknown_value()->location();

    case NAMED_OBJECT_CONST:
      return this->const_value()->location();

    case NAMED_OBJECT_TYPE:
      return this->type_value()->location();

    case NAMED_OBJECT_TYPE_DECLARATION:
      return this->type_declaration_value()->location();

    case NAMED_OBJECT_VAR:
      return this->var_value()->location();

    case NAMED_OBJECT_RESULT_VAR:
      return this->result_var_value()->location();

    case NAMED_OBJECT_SINK:
      go_unreachable();

    case NAMED_OBJECT_FUNC:
      return this->func_value()->location();

    case NAMED_OBJECT_FUNC_DECLARATION:
      return this->func_declaration_value()->location();

    case NAMED_OBJECT_PACKAGE:
      return this->package_value()->location();
    }
}

// Traverse a Named_object.

int
Named_object::traverse(Traverse* traverse, bool is_global)
{
  const unsigned int traverse_mask = traverse->traverse_mask();
  const unsigned int e_or_t = (Traverse::traverse_expressions
			       | Traverse::traverse_types);
  const unsigned int e_or_t_or_s = (e_or_t
				    | Traverse::traverse_statements);

  int t = TRAVERSE_CONTINUE;
  switch (this->classification_)
    {
    case Named_object::NAMED_OBJECT_CONST:
      if ((traverse_mask & Traverse::traverse_constants) != 0)
	t = traverse->constant(this, is_global);
      if (t == TRAVERSE_CONTINUE
	  && (traverse_mask & e_or_t) != 0)
	{
	  Type* tc = this->const_value()->type();
	  if (tc != NULL)
	    {
	      if (Type::traverse(tc, traverse) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	  t = this->const_value()->traverse_expression(traverse);
	}
      break;

    case Named_object::NAMED_OBJECT_VAR:
    case Named_object::NAMED_OBJECT_RESULT_VAR:
      if ((traverse_mask & Traverse::traverse_variables) != 0)
	t = traverse->variable(this);
      if (t == TRAVERSE_CONTINUE
	  && (traverse_mask & e_or_t) != 0)
	{
	  if (this->is_result_variable() || this->var_value()->has_type())
	    {
	      Type* tv = (this->is_variable()
			  ? this->var_value()->type()
			  : this->result_var_value()->type());
	      if (tv != NULL)
		{
		  if (Type::traverse(tv, traverse) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		}
	    }
	}
      if (t == TRAVERSE_CONTINUE
	  && (traverse_mask & e_or_t_or_s) != 0
	  && this->is_variable())
	t = this->var_value()->traverse_expression(traverse,
						   traverse_mask);
      break;

    case Named_object::NAMED_OBJECT_FUNC:
      if ((traverse_mask & Traverse::traverse_functions) != 0)
	t = traverse->function(this);
      if (t == TRAVERSE_CONTINUE
	  && (traverse_mask
	      & (Traverse::traverse_variables
		 | Traverse::traverse_constants
		 | Traverse::traverse_functions
		 | Traverse::traverse_blocks
		 | Traverse::traverse_statements
		 | Traverse::traverse_expressions
		 | Traverse::traverse_types)) != 0)
	t = this->func_value()->traverse(traverse);
      break;

    case Named_object::NAMED_OBJECT_TYPE:
      if ((traverse_mask & e_or_t) != 0)
	t = Type::traverse(this->type_value(), traverse);
      break;

    case Named_object::NAMED_OBJECT_PACKAGE:
    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
    case Named_object::NAMED_OBJECT_UNKNOWN:
    case Named_object::NAMED_OBJECT_ERRONEOUS:
      break;

    case Named_object::NAMED_OBJECT_SINK:
      go_unreachable();

    default:
      go_unreachable();
    }

  return t;
}

// Export a named object.

void
Named_object::export_named_object(Export* exp) const
{
  switch (this->classification_)
    {
    default:
    case NAMED_OBJECT_UNINITIALIZED:
    case NAMED_OBJECT_UNKNOWN:
      go_unreachable();

    case NAMED_OBJECT_ERRONEOUS:
      break;

    case NAMED_OBJECT_CONST:
      this->const_value()->export_const(exp, this->name_);
      break;

    case NAMED_OBJECT_TYPE:
      // Types are handled by export::write_types.
      go_unreachable();

    case NAMED_OBJECT_TYPE_DECLARATION:
      go_error_at(this->type_declaration_value()->location(),
		  "attempt to export %<%s%> which was declared but not defined",
		  this->message_name().c_str());
      break;

    case NAMED_OBJECT_FUNC_DECLARATION:
      this->func_declaration_value()->export_func(exp, this);
      break;

    case NAMED_OBJECT_VAR:
      this->var_value()->export_var(exp, this);
      break;

    case NAMED_OBJECT_RESULT_VAR:
    case NAMED_OBJECT_SINK:
      go_unreachable();

    case NAMED_OBJECT_FUNC:
      this->func_value()->export_func(exp, this);
      break;
    }
}

// Convert a variable to the backend representation.

Bvariable*
Named_object::get_backend_variable(Gogo* gogo, Named_object* function)
{
  if (this->classification_ == NAMED_OBJECT_VAR)
    return this->var_value()->get_backend_variable(gogo, function,
						   this->package_, this->name_);
  else if (this->classification_ == NAMED_OBJECT_RESULT_VAR)
    return this->result_var_value()->get_backend_variable(gogo, function,
							  this->name_);
  else
    go_unreachable();
}

void
debug_go_named_object(Named_object* no)
{
  if (no == NULL)
    {
      std::cerr << "<null>";
      return;
    }
  std::cerr << "'" << no->name() << "': ";
  const char *tag;
  switch (no->classification())
    {
      case Named_object::NAMED_OBJECT_UNINITIALIZED:
        tag = "uninitialized";
        break;
      case Named_object::NAMED_OBJECT_ERRONEOUS:
        tag = "<error>";
        break;
      case Named_object::NAMED_OBJECT_UNKNOWN:
        tag = "<unknown>";
        break;
      case Named_object::NAMED_OBJECT_CONST:
        tag = "constant";
        break;
      case Named_object::NAMED_OBJECT_TYPE:
        tag = "type";
        break;
      case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
        tag = "type_decl";
        break;
      case Named_object::NAMED_OBJECT_VAR:
        tag = "var";
        break;
      case Named_object::NAMED_OBJECT_RESULT_VAR:
        tag = "result_var";
        break;
      case Named_object::NAMED_OBJECT_SINK:
        tag = "<sink>";
        break;
      case Named_object::NAMED_OBJECT_FUNC:
        tag = "func";
        break;
      case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
        tag = "func_decl";
        break;
      case Named_object::NAMED_OBJECT_PACKAGE:
        tag = "package";
        break;
      default:
        tag = "<unknown named object classification>";
        break;
  };
  std::cerr << tag << "\n";
}

// Get the backend representation for this named object.

void
Named_object::get_backend(Gogo* gogo, std::vector<Bexpression*>& const_decls,
                          std::vector<Btype*>& type_decls,
                          std::vector<Bfunction*>& func_decls)
{
  // If this is a definition, avoid trying to get the backend
  // representation, as that can crash.
  if (this->is_redefinition_)
    {
      go_assert(saw_errors());
      return;
    }

  switch (this->classification_)
    {
    case NAMED_OBJECT_CONST:
      if (!Gogo::is_erroneous_name(this->name_))
	const_decls.push_back(this->u_.const_value->get_backend(gogo, this));
      break;

    case NAMED_OBJECT_TYPE:
      {
        Named_type* named_type = this->u_.type_value;

        // No need to do anything for aliases-- whatever has to be done
        // can be done for the alias target.
        if (named_type->is_alias())
          break;

	if (!Gogo::is_erroneous_name(this->name_))
	  type_decls.push_back(named_type->get_backend(gogo));

        // We need to produce a type descriptor for every named
        // type, and for a pointer to every named type, since
        // other files or packages might refer to them.  We need
        // to do this even for hidden types, because they might
        // still be returned by some function.  Simply calling the
        // type_descriptor method is enough to create the type
        // descriptor, even though we don't do anything with it.
        if (this->package_ == NULL && !saw_errors())
          {
            named_type->
                type_descriptor_pointer(gogo, Linemap::predeclared_location());
            Type* pn = Type::make_pointer_type(named_type);
            pn->type_descriptor_pointer(gogo, Linemap::predeclared_location());
	    if (named_type->in_heap())
	      {
		named_type->gc_symbol_pointer(gogo);
		pn->gc_symbol_pointer(gogo);
	      }
          }
      }
      break;

    case NAMED_OBJECT_TYPE_DECLARATION:
      go_error_at(Linemap::unknown_location(),
		  "reference to undefined type %qs",
		  this->message_name().c_str());
      return;

    case NAMED_OBJECT_VAR:
    case NAMED_OBJECT_RESULT_VAR:
    case NAMED_OBJECT_SINK:
      go_unreachable();

    case NAMED_OBJECT_FUNC:
      {
	Function* func = this->u_.func_value;
	if (!Gogo::is_erroneous_name(this->name_))
	  func_decls.push_back(func->get_or_make_decl(gogo, this));

	if (func->block() != NULL)
	  func->build(gogo, this);
      }
      break;

    case NAMED_OBJECT_ERRONEOUS:
      break;

    default:
      go_unreachable();
    }
}

// Class Bindings.

Bindings::Bindings(Bindings* enclosing)
  : enclosing_(enclosing), named_objects_(), bindings_()
{
}

// Clear imports.

void
Bindings::clear_file_scope(Gogo* gogo)
{
  Contour::iterator p = this->bindings_.begin();
  while (p != this->bindings_.end())
    {
      bool keep;
      if (p->second->package() != NULL)
	keep = false;
      else if (p->second->is_package())
	keep = false;
      else if (p->second->is_function()
	       && !p->second->func_value()->type()->is_method()
	       && Gogo::unpack_hidden_name(p->second->name()) == "init")
	keep = false;
      else
	keep = true;

      if (keep)
	++p;
      else
	{
	  gogo->add_file_block_name(p->second->name(), p->second->location());
	  p = this->bindings_.erase(p);
	}
    }
}

// Look up a symbol.

Named_object*
Bindings::lookup(const std::string& name) const
{
  Contour::const_iterator p = this->bindings_.find(name);
  if (p != this->bindings_.end())
    return p->second->resolve();
  else if (this->enclosing_ != NULL)
    return this->enclosing_->lookup(name);
  else
    return NULL;
}

// Look up a symbol locally.

Named_object*
Bindings::lookup_local(const std::string& name) const
{
  Contour::const_iterator p = this->bindings_.find(name);
  if (p == this->bindings_.end())
    return NULL;
  return p->second;
}

// Remove an object from a set of bindings.  This is used for a
// special case in thunks for functions which call recover.

void
Bindings::remove_binding(Named_object* no)
{
  Contour::iterator pb = this->bindings_.find(no->name());
  go_assert(pb != this->bindings_.end());
  this->bindings_.erase(pb);
  for (std::vector<Named_object*>::iterator pn = this->named_objects_.begin();
       pn != this->named_objects_.end();
       ++pn)
    {
      if (*pn == no)
	{
	  this->named_objects_.erase(pn);
	  return;
	}
    }
  go_unreachable();
}

// Add a method to the list of objects.  This is not added to the
// lookup table.  This is so that we have a single list of objects
// declared at the top level, which we walk through when it's time to
// convert to trees.

void
Bindings::add_method(Named_object* method)
{
  this->named_objects_.push_back(method);
}

// Add a generic Named_object to a Contour.

Named_object*
Bindings::add_named_object_to_contour(Contour* contour,
				      Named_object* named_object)
{
  go_assert(named_object == named_object->resolve());
  const std::string& name(named_object->name());
  go_assert(!Gogo::is_sink_name(name));

  std::pair<Contour::iterator, bool> ins =
    contour->insert(std::make_pair(name, named_object));
  if (!ins.second)
    {
      // The name was already there.
      if (named_object->package() != NULL
	  && ins.first->second->package() == named_object->package()
	  && (ins.first->second->classification()
	      == named_object->classification()))
	{
	  // This is a second import of the same object.
	  return ins.first->second;
	}
      ins.first->second = this->new_definition(ins.first->second,
					       named_object);
      return ins.first->second;
    }
  else
    {
      // Don't push declarations on the list.  We push them on when
      // and if we find the definitions.  That way we genericize the
      // functions in order.
      if (!named_object->is_type_declaration()
	  && !named_object->is_function_declaration()
	  && !named_object->is_unknown())
	this->named_objects_.push_back(named_object);
      return named_object;
    }
}

// We had an existing named object OLD_OBJECT, and we've seen a new
// one NEW_OBJECT with the same name.  FIXME: This does not free the
// new object when we don't need it.

Named_object*
Bindings::new_definition(Named_object* old_object, Named_object* new_object)
{
  if (new_object->is_erroneous() && !old_object->is_erroneous())
    return new_object;

  std::string reason;
  switch (old_object->classification())
    {
    default:
    case Named_object::NAMED_OBJECT_UNINITIALIZED:
      go_unreachable();

    case Named_object::NAMED_OBJECT_ERRONEOUS:
      return old_object;

    case Named_object::NAMED_OBJECT_UNKNOWN:
      {
	Named_object* real = old_object->unknown_value()->real_named_object();
	if (real != NULL)
	  return this->new_definition(real, new_object);
	go_assert(!new_object->is_unknown());
	old_object->unknown_value()->set_real_named_object(new_object);
	if (!new_object->is_type_declaration()
	    && !new_object->is_function_declaration())
	  this->named_objects_.push_back(new_object);
	return new_object;
      }

    case Named_object::NAMED_OBJECT_CONST:
      break;

    case Named_object::NAMED_OBJECT_TYPE:
      if (new_object->is_type_declaration())
	return old_object;
      break;

    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
      if (new_object->is_type_declaration())
	return old_object;
      if (new_object->is_type())
	{
	  old_object->set_type_value(new_object->type_value());
	  new_object->type_value()->set_named_object(old_object);
	  this->named_objects_.push_back(old_object);
	  return old_object;
	}
      break;

    case Named_object::NAMED_OBJECT_VAR:
    case Named_object::NAMED_OBJECT_RESULT_VAR:
      // We have already given an error in the parser for cases where
      // one parameter or result variable redeclares another one.
      if ((new_object->is_variable()
	   && new_object->var_value()->is_parameter())
	  || new_object->is_result_variable())
	return old_object;
      break;

    case Named_object::NAMED_OBJECT_SINK:
      go_unreachable();

    case Named_object::NAMED_OBJECT_FUNC:
      break;

    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      {
	// We declare the hash and equality functions before defining
	// them, because we sometimes see that we need the declaration
	// while we are in the middle of a different function.
	//
	// We declare the main function before the user defines it, to
	// give better error messages.
	//
	// We declare inline functions before we define them, as we
	// only define them if we need them.
	if (new_object->is_function()
	    && ((Linemap::is_predeclared_location(old_object->location())
		 && Linemap::is_predeclared_location(new_object->location()))
		|| (Gogo::unpack_hidden_name(old_object->name()) == "main"
		    && Linemap::is_unknown_location(old_object->location()))
		|| (new_object->package() != NULL
		    && old_object->func_declaration_value()->has_imported_body()
		    && new_object->func_value()->is_inline_only())))
	  {
            Function_type* old_type =
                old_object->func_declaration_value()->type();
	    Function_type* new_type = new_object->func_value()->type();
	    if (old_type->is_valid_redeclaration(new_type, &reason))
	      {
		Function_declaration* fd =
		  old_object->func_declaration_value();
		go_assert(fd->asm_name().empty());
		old_object->set_function_value(new_object->func_value());
		this->named_objects_.push_back(old_object);
		return old_object;
	      }
	  }
      }
      break;

    case Named_object::NAMED_OBJECT_PACKAGE:
      break;
    }

  std::string n = old_object->message_name();
  if (reason.empty())
    go_error_at(new_object->location(), "redefinition of %qs", n.c_str());
  else
    go_error_at(new_object->location(), "redefinition of %qs: %s", n.c_str(),
		reason.c_str());
  old_object->set_is_redefinition();
  new_object->set_is_redefinition();

  if (!Linemap::is_unknown_location(old_object->location())
      && !Linemap::is_predeclared_location(old_object->location()))
    go_inform(old_object->location(), "previous definition of %qs was here",
	      n.c_str());

  return old_object;
}

// Add a named type.

Named_object*
Bindings::add_named_type(Named_type* named_type)
{
  return this->add_named_object(named_type->named_object());
}

// Add a function.

Named_object*
Bindings::add_function(const std::string& name, const Package* package,
		       Function* function)
{
  return this->add_named_object(Named_object::make_function(name, package,
							    function));
}

// Add a function declaration.

Named_object*
Bindings::add_function_declaration(const std::string& name,
				   const Package* package,
				   Function_type* type,
				   Location location)
{
  Named_object* no = Named_object::make_function_declaration(name, package,
							     type, location);
  return this->add_named_object(no);
}

// Define a type which was previously declared.

void
Bindings::define_type(Named_object* no, Named_type* type)
{
  no->set_type_value(type);
  this->named_objects_.push_back(no);
}

// Mark all local variables as used.  This is used for some types of
// parse error.

void
Bindings::mark_locals_used()
{
  for (std::vector<Named_object*>::iterator p = this->named_objects_.begin();
       p != this->named_objects_.end();
       ++p)
    if ((*p)->is_variable())
      (*p)->var_value()->set_is_used();
}

// Traverse bindings.

int
Bindings::traverse(Traverse* traverse, bool is_global)
{
  unsigned int traverse_mask = traverse->traverse_mask();

  // We don't use an iterator because we permit the traversal to add
  // new global objects.
  const unsigned int e_or_t = (Traverse::traverse_expressions
			       | Traverse::traverse_types);
  for (size_t i = 0; i < this->named_objects_.size(); ++i)
    {
      Named_object* p = this->named_objects_[i];
      if (p->traverse(traverse, is_global) == TRAVERSE_EXIT)
	return TRAVERSE_EXIT;
    }

  // If we need to traverse types, check the function declarations,
  // which have types.  Also check any methods of a type declaration.
  if ((traverse_mask & e_or_t) != 0)
    {
      for (Bindings::const_declarations_iterator p =
	     this->begin_declarations();
	   p != this->end_declarations();
	   ++p)
	{
	  if (p->second->is_function_declaration())
	    {
	      if (Type::traverse(p->second->func_declaration_value()->type(),
				 traverse)
		  == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	    }
	  else if (p->second->is_type_declaration())
	    {
	      const std::vector<Named_object*>* methods =
		p->second->type_declaration_value()->methods();
	      for (std::vector<Named_object*>::const_iterator pm =
		     methods->begin();
		   pm != methods->end();
		   pm++)
		{
		  Named_object* no = *pm;
		  Type *t;
		  if (no->is_function())
		    t = no->func_value()->type();
		  else if (no->is_function_declaration())
		    t = no->func_declaration_value()->type();
		  else
		    continue;
		  if (Type::traverse(t, traverse) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		}
	    }
	}
    }

  // Traverse function declarations when needed.
  if ((traverse_mask & Traverse::traverse_func_declarations) != 0)
    {
      for (Bindings::const_declarations_iterator p = this->begin_declarations();
           p != this->end_declarations();
           ++p)
        {
          if (p->second->is_function_declaration())
            {
              if (traverse->function_declaration(p->second) == TRAVERSE_EXIT)
                return TRAVERSE_EXIT;
            }
        }
    }

  return TRAVERSE_CONTINUE;
}

void
Bindings::debug_dump()
{
  std::set<Named_object*> defs;
  for (size_t i = 0; i < this->named_objects_.size(); ++i)
    defs.insert(this->named_objects_[i]);
  for (Contour::iterator p = this->bindings_.begin();
       p != this->bindings_.end();
       ++p)
    {
      const char* tag = "  ";
      if (defs.find(p->second) != defs.end())
        tag = "* ";
      std::cerr << tag;
      debug_go_named_object(p->second);
    }
}

void
debug_go_bindings(Bindings* bindings)
{
  if (bindings != NULL)
    bindings->debug_dump();
}

// Class Label.

// Clear any references to this label.

void
Label::clear_refs()
{
  for (std::vector<Bindings_snapshot*>::iterator p = this->refs_.begin();
       p != this->refs_.end();
       ++p)
    delete *p;
  this->refs_.clear();
}

// Get the backend representation for a label.

Blabel*
Label::get_backend_label(Translate_context* context)
{
  if (this->blabel_ == NULL)
    {
      Function* function = context->function()->func_value();
      Bfunction* bfunction = function->get_decl();
      this->blabel_ = context->backend()->label(bfunction, this->name_,
						this->location_);
    }
  return this->blabel_;
}

// Return an expression for the address of this label.

Bexpression*
Label::get_addr(Translate_context* context, Location location)
{
  Blabel* label = this->get_backend_label(context);
  return context->backend()->label_address(label, location);
}

// Return the dummy label that represents any instance of the blank label.

Label*
Label::create_dummy_label()
{
  static Label* dummy_label;
  if (dummy_label == NULL)
    {
      dummy_label = new Label("_");
      dummy_label->set_is_used();
    }
  return dummy_label;
}

// Class Unnamed_label.

// Get the backend representation for an unnamed label.

Blabel*
Unnamed_label::get_blabel(Translate_context* context)
{
  if (this->blabel_ == NULL)
    {
      Function* function = context->function()->func_value();
      Bfunction* bfunction = function->get_decl();
      this->blabel_ = context->backend()->label(bfunction, "",
						this->location_);
    }
  return this->blabel_;
}

// Return a statement which defines this unnamed label.

Bstatement*
Unnamed_label::get_definition(Translate_context* context)
{
  Blabel* blabel = this->get_blabel(context);
  return context->backend()->label_definition_statement(blabel);
}

// Return a goto statement to this unnamed label.

Bstatement*
Unnamed_label::get_goto(Translate_context* context, Location location)
{
  Blabel* blabel = this->get_blabel(context);
  return context->backend()->goto_statement(blabel, location);
}

// Class Package.

Package::Package(const std::string& pkgpath,
		 const std::string& pkgpath_symbol, Location location)
  : pkgpath_(pkgpath), pkgpath_symbol_(pkgpath_symbol),
    package_name_(), bindings_(new Bindings(NULL)),
    location_(location)
{
  go_assert(!pkgpath.empty());
}

// Set the package name.

void
Package::set_package_name(const std::string& package_name, Location location)
{
  go_assert(!package_name.empty());
  if (this->package_name_.empty())
    this->package_name_ = package_name;
  else if (this->package_name_ != package_name)
    go_error_at(location,
		("saw two different packages with "
		 "the same package path %s: %s, %s"),
		this->pkgpath_.c_str(), this->package_name_.c_str(),
		package_name.c_str());
}

// Return the pkgpath symbol, which is a prefix for symbols defined in
// this package.

std::string
Package::pkgpath_symbol() const
{
  if (this->pkgpath_symbol_.empty())
    return Gogo::pkgpath_for_symbol(this->pkgpath_);
  return this->pkgpath_symbol_;
}

// Set the package path symbol.

void
Package::set_pkgpath_symbol(const std::string& pkgpath_symbol)
{
  go_assert(!pkgpath_symbol.empty());
  if (this->pkgpath_symbol_.empty())
    this->pkgpath_symbol_ = pkgpath_symbol;
  else
    go_assert(this->pkgpath_symbol_ == pkgpath_symbol);
}

// Note that symbol from this package was and qualified by ALIAS.

void
Package::note_usage(const std::string& alias) const
{
  Aliases::const_iterator p = this->aliases_.find(alias);
  go_assert(p != this->aliases_.end());
  p->second->note_usage();
}

// Forget a given usage.  If forgetting this usage means this package becomes
// unused, report that error.

void
Package::forget_usage(Expression* usage) const
{
  if (this->fake_uses_.empty())
    return;

  std::set<Expression*>::iterator p = this->fake_uses_.find(usage);
  go_assert(p != this->fake_uses_.end());
  this->fake_uses_.erase(p);

  if (this->fake_uses_.empty())
    go_error_at(this->location(), "imported and not used: %s",
		Gogo::message_name(this->package_name()).c_str());
}

// Clear the used field for the next file.  If the only usages of this package
// are possibly fake, keep the fake usages for lowering.

void
Package::clear_used()
{
  std::string dot_alias = "." + this->package_name();
  Aliases::const_iterator p = this->aliases_.find(dot_alias);
  if (p != this->aliases_.end() && p->second->used() > this->fake_uses_.size())
    this->fake_uses_.clear();

  this->aliases_.clear();
}

Package_alias*
Package::add_alias(const std::string& alias, Location location)
{
  Aliases::const_iterator p = this->aliases_.find(alias);
  if (p == this->aliases_.end())
    {
      std::pair<Aliases::iterator, bool> ret;
      ret = this->aliases_.insert(std::make_pair(alias,
                                                 new Package_alias(location)));
      p = ret.first;
    }
  return p->second;
}

// Determine types of constants.  Everything else in a package
// (variables, function declarations) should already have a fixed
// type.  Constants may have abstract types.

void
Package::determine_types()
{
  Bindings* bindings = this->bindings_;
  for (Bindings::const_definitions_iterator p = bindings->begin_definitions();
       p != bindings->end_definitions();
       ++p)
    {
      if ((*p)->is_const())
	(*p)->const_value()->determine_type();
    }
}

// Class Traverse.

// Destructor.

Traverse::~Traverse()
{
  if (this->types_seen_ != NULL)
    delete this->types_seen_;
  if (this->expressions_seen_ != NULL)
    delete this->expressions_seen_;
}

// Record that we are looking at a type, and return true if we have
// already seen it.

bool
Traverse::remember_type(const Type* type)
{
  if (type->is_error_type())
    return true;
  go_assert((this->traverse_mask() & traverse_types) != 0
	     || (this->traverse_mask() & traverse_expressions) != 0);
  // We mostly only have to remember named types.  But it turns out
  // that an interface type can refer to itself without using a name
  // by relying on interface inheritance, as in
  //
  //         type I interface { F() interface{I} }
  //
  // Similarly it is possible for array types to refer to themselves
  // without a name, e.g.
  //
  //         var x [uintptr(unsafe.Sizeof(&x))]byte
  //
  if (type->classification() != Type::TYPE_NAMED
      && type->classification() != Type::TYPE_ARRAY
      && type->classification() != Type::TYPE_INTERFACE)
    return false;
  if (this->types_seen_ == NULL)
    this->types_seen_ = new Types_seen();
  std::pair<Types_seen::iterator, bool> ins = this->types_seen_->insert(type);
  return !ins.second;
}

// Record that we are looking at an expression, and return true if we
// have already seen it. NB: this routine used to assert if the traverse
// mask did not include expressions/types -- this is no longer the case,
// since it can be useful to remember specific expressions during
// walks that only cover statements.

bool
Traverse::remember_expression(const Expression* expression)
{
  if (this->expressions_seen_ == NULL)
    this->expressions_seen_ = new Expressions_seen();
  std::pair<Expressions_seen::iterator, bool> ins =
    this->expressions_seen_->insert(expression);
  return !ins.second;
}

// The default versions of these functions should never be called: the
// traversal mask indicates which functions may be called.

int
Traverse::variable(Named_object*)
{
  go_unreachable();
}

int
Traverse::constant(Named_object*, bool)
{
  go_unreachable();
}

int
Traverse::function(Named_object*)
{
  go_unreachable();
}

int
Traverse::block(Block*)
{
  go_unreachable();
}

int
Traverse::statement(Block*, size_t*, Statement*)
{
  go_unreachable();
}

int
Traverse::expression(Expression**)
{
  go_unreachable();
}

int
Traverse::type(Type*)
{
  go_unreachable();
}

int
Traverse::function_declaration(Named_object*)
{
  go_unreachable();
}

// Class Statement_inserter.

void
Statement_inserter::insert(Statement* s)
{
  if (this->statements_added_ != NULL)
    this->statements_added_->insert(s);

  if (this->block_ != NULL)
    {
      go_assert(this->pindex_ != NULL);
      this->block_->insert_statement_before(*this->pindex_, s);
      ++*this->pindex_;
    }
  else if (this->var_ != NULL)
    this->var_->add_preinit_statement(this->gogo_, s);
  else
    go_assert(saw_errors());
}
