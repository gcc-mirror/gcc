// gogo.cc -- Go frontend parsed representation.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "filenames.h"

#include "go-c.h"
#include "go-dump.h"
#include "lex.h"
#include "types.h"
#include "statements.h"
#include "expressions.h"
#include "dataflow.h"
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
    verify_types_(),
    interface_types_(),
    specific_type_functions_(),
    specific_type_functions_are_written_(false),
    named_types_are_converted_(false)
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
  Named_object* byte_type = Named_object::make_type("byte", NULL, uint8_type,
						    loc);
  this->add_named_type(byte_type->type_value());

  // "rune" is an alias for "int32".
  int32_type->integer_type()->set_is_rune();
  Named_object* rune_type = Named_object::make_type("rune", NULL, int32_type,
						    loc);
  this->add_named_type(rune_type->type_value());

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

// Convert a pkgpath into a string suitable for a symbol.  Note that
// this transformation is convenient but imperfect.  A -fgo-pkgpath
// option of a/b_c will conflict with a -fgo-pkgpath option of a_b/c,
// possibly leading to link time errors.

std::string
Gogo::pkgpath_for_symbol(const std::string& pkgpath)
{
  std::string s = pkgpath;
  for (size_t i = 0; i < s.length(); ++i)
    {
      char c = s[i];
      if ((c >= 'a' && c <= 'z')
	  || (c >= 'A' && c <= 'Z')
	  || (c >= '0' && c <= '9')
	  || c == '_'
	  || c == '.'
	  || c == '$')
	;
      else
	s[i] = '_';
    }
  return s;
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
	error_at(location, "expected package %<%s%>",
		 Gogo::message_name(this->package_->package_name()).c_str());
      return;
    }

  // Now that we know the name of the package we are compiling, set
  // the package path to use for reflect.Type.PkgPath and global
  // symbol names.
  if (!this->pkgpath_set_)
    {
      if (!this->prefix_from_option_ && package_name == "main")
	this->pkgpath_ = package_name;
      else
	{
	  if (!this->prefix_from_option_)
	    this->prefix_ = "go";
	  this->pkgpath_ = this->prefix_ + '.' + package_name;
	}
      this->pkgpath_set_ = true;
    }

  this->pkgpath_symbol_ = Gogo::pkgpath_for_symbol(this->pkgpath_);

  this->package_ = this->register_package(this->pkgpath_, location);
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
		     Location location)
{
  if (filename.empty())
    {
      error_at(location, "import path is empty");
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
	  error_at(location, "import path contains invalid UTF-8 sequence");
	  return;
	}
      if (c == '\0')
	{
	  error_at(location, "import path contains NUL");
	  return;
	}
      if (c < 0x20 || c == 0x7f)
	{
	  error_at(location, "import path contains control character");
	  return;
	}
      if (c == '\\')
	{
	  error_at(location, "import path contains backslash; use slash");
	  return;
	}
      if (Lex::is_unicode_space(c))
	{
	  error_at(location, "import path contains space character");
	  return;
	}
      if (c < 0x7f && strchr("!\"#$%&'()*,:;<=>?[]^`{|}", c) != NULL)
	{
	  error_at(location, "import path contains invalid character '%c'", c);
	  return;
	}
      pf += adv;
    }

  if (IS_ABSOLUTE_PATH(filename.c_str()))
    {
      error_at(location, "import path cannot be absolute path");
      return;
    }

  if (filename == "unsafe")
    {
      this->import_unsafe(local_name, is_local_name_exported, location);
      return;
    }

  Imports::const_iterator p = this->imports_.find(filename);
  if (p != this->imports_.end())
    {
      Package* package = p->second;
      package->set_location(location);
      package->set_is_imported();
      std::string ln = local_name;
      bool is_ln_exported = is_local_name_exported;
      if (ln.empty())
	{
	  ln = package->package_name();
	  go_assert(!ln.empty());
	  is_ln_exported = Lex::is_exported_name(ln);
	}
      if (ln == ".")
	{
	  Bindings* bindings = package->bindings();
	  for (Bindings::const_declarations_iterator p =
		 bindings->begin_declarations();
	       p != bindings->end_declarations();
	       ++p)
	    this->add_named_object(p->second);
	}
      else if (ln == "_")
	package->set_uses_sink_alias();
      else
	{
	  ln = this->pack_hidden_name(ln, is_ln_exported);
	  this->package_->bindings()->add_package(ln, package);
	}
      return;
    }

  Import::Stream* stream = Import::open_package(filename, location,
						this->relative_import_path_);
  if (stream == NULL)
    {
      error_at(location, "import file %qs not found", filename.c_str());
      return;
    }

  Import imp(stream, location);
  imp.register_builtin_types(this);
  Package* package = imp.import(this, local_name, is_local_name_exported);
  if (package != NULL)
    {
      if (package->pkgpath() == this->pkgpath())
	error_at(location,
		 ("imported package uses same package path as package "
		  "being compiled (see -fgo-pkgpath option)"));

      this->imports_.insert(std::make_pair(filename, package));
      package->set_is_imported();
    }

  delete stream;
}

// Add an import control function for an imported package to the list.

void
Gogo::add_import_init_fn(const std::string& package_name,
			 const std::string& init_name, int prio)
{
  for (std::set<Import_init>::const_iterator p =
	 this->imported_init_fns_.begin();
       p != this->imported_init_fns_.end();
       ++p)
    {
      if (p->init_name() == init_name)
	{
	  // If a test of package P1, built as part of package P1,
	  // imports package P2, and P2 imports P1 (perhaps
	  // indirectly), then we will see the same import name with
	  // different import priorities.  That is OK, so don't give
	  // an error about it.
	  if (p->package_name() != package_name)
	    {
	      error("duplicate package initialization name %qs",
		    Gogo::message_name(init_name).c_str());
	      inform(UNKNOWN_LOCATION, "used by package %qs at priority %d",
		     Gogo::message_name(p->package_name()).c_str(),
		     p->priority());
	      inform(UNKNOWN_LOCATION, " and by package %qs at priority %d",
		     Gogo::message_name(package_name).c_str(), prio);
	    }
	  return;
	}
    }

  this->imported_init_fns_.insert(Import_init(package_name, init_name,
					      prio));
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
	    ret->package()->set_used();
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
			   Location location,
			   bool* padd_to_globals)
{
  Package* ret = this->register_package(pkgpath, location);
  ret->set_package_name(real_name, location);

  *padd_to_globals = false;

  if (alias_arg == ".")
    *padd_to_globals = true;
  else if (alias_arg == "_")
    ret->set_uses_sink_alias();
  else
    {
      std::string alias = alias_arg;
      if (alias.empty())
	{
	  alias = real_name;
	  is_alias_exported = Lex::is_exported_name(alias);
	}
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
// led us to see this package.

Package*
Gogo::register_package(const std::string& pkgpath, Location location)
{
  Package* package = NULL;
  std::pair<Packages::iterator, bool> ins =
    this->packages_.insert(std::make_pair(pkgpath, package));
  if (!ins.second)
    {
      // We have seen this package name before.
      package = ins.first->second;
      go_assert(package != NULL && package->pkgpath() == pkgpath);
      if (Linemap::is_unknown_location(package->location()))
	package->set_location(location);
    }
  else
    {
      // First time we have seen this package name.
      package = new Package(pkgpath, location);
      go_assert(ins.first->second == NULL);
      ins.first->second = package;
    }

  return package;
}

// Start compiling a function.

Named_object*
Gogo::start_function(const std::string& name, Function_type* type,
		     bool add_method_to_type, Location location)
{
  bool at_top_level = this->functions_.empty();

  Block* block = new Block(NULL, location);

  Function* enclosing = (at_top_level
			 ? NULL
			 : this->functions_.back().function->func_value());

  Function* function = new Function(type, enclosing, block, location);

  if (type->is_method())
    {
      const Typed_identifier* receiver = type->receiver();
      Variable* this_param = new Variable(receiver->type(), NULL, false,
					  true, true, location);
      std::string rname = receiver->name();
      if (rname.empty() || Gogo::is_sink_name(rname))
	{
	  // We need to give receivers a name since they wind up in
	  // DECL_ARGUMENTS.  FIXME.
	  static unsigned int count;
	  char buf[50];
	  snprintf(buf, sizeof buf, "r.%u", count);
	  ++count;
	  rname = buf;
	}
      block->bindings()->add_variable(rname, NULL, this_param);
    }

  const Typed_identifier_list* parameters = type->parameters();
  bool is_varargs = type->is_varargs();
  if (parameters != NULL)
    {
      for (Typed_identifier_list::const_iterator p = parameters->begin();
	   p != parameters->end();
	   ++p)
	{
	  Variable* param = new Variable(p->type(), NULL, false, true, false,
					 location);
	  if (is_varargs && p + 1 == parameters->end())
	    param->set_is_varargs_parameter();

	  std::string pname = p->name();
	  if (pname.empty() || Gogo::is_sink_name(pname))
	    {
	      // We need to give parameters a name since they wind up
	      // in DECL_ARGUMENTS.  FIXME.
	      static unsigned int count;
	      char buf[50];
	      snprintf(buf, sizeof buf, "p.%u", count);
	      ++count;
	      pname = buf;
	    }
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
	error_at(location,
		 "func init must have no arguments and no return values");
      // There can be multiple "init" functions, so give them each a
      // different name.
      static int init_count;
      char buf[30];
      snprintf(buf, sizeof buf, ".$init%d", init_count);
      ++init_count;
      nested_name = buf;
      pname = &nested_name;
      is_init = true;
    }
  else if (!name.empty())
    pname = &name;
  else
    {
      // Invent a name for a nested function.
      static int nested_count;
      char buf[30];
      snprintf(buf, sizeof buf, ".$nested%d", nested_count);
      ++nested_count;
      nested_name = buf;
      pname = &nested_name;
    }

  Named_object* ret;
  if (Gogo::is_sink_name(*pname))
    {
      static int sink_count;
      char buf[30];
      snprintf(buf, sizeof buf, ".$sink%d", sink_count);
      ++sink_count;
      ret = this->package_->bindings()->add_function(buf, NULL, function);
      ret->func_value()->set_is_sink();
    }
  else if (!type->is_method())
    {
      ret = this->package_->bindings()->add_function(*pname, NULL, function);
      if (!ret->is_function() || ret->func_value() != function)
	{
	  // Redefinition error.  Invent a name to avoid knockon
	  // errors.
	  static int redefinition_count;
	  char buf[30];
	  snprintf(buf, sizeof buf, ".$redefined%d", redefinition_count);
	  ++redefinition_count;
	  ret = this->package_->bindings()->add_function(buf, NULL, function);
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

	  // We want to look through the pointer created by the
	  // parser, without getting an error if the type is not yet
	  // defined.
	  if (rtype->classification() == Type::TYPE_POINTER)
	    rtype = rtype->points_to();

	  if (rtype->is_error_type())
	    ret = Named_object::make_function(name, NULL, function);
	  else if (rtype->named_type() != NULL)
	    {
	      ret = rtype->named_type()->add_method(name, function);
	      if (!ret->is_function())
		{
		  // Redefinition error.
		  ret = Named_object::make_function(name, NULL, function);
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
	    go_unreachable();
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

      // We want to look through the pointer created by the
      // parser, without getting an error if the type is not yet
      // defined.
      if (rtype->classification() == Type::TYPE_POINTER)
	rtype = rtype->points_to();

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
	go_unreachable();
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

// Add a sink--a reference to the blank identifier _.

Named_object*
Gogo::add_sink()
{
  return Named_object::make_sink();
}

// Add a named object.

void
Gogo::add_named_object(Named_object* no)
{
  this->current_bindings()->add_named_object(no);
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

// Return a name for a thunk object.

std::string
Gogo::thunk_name()
{
  static int thunk_count;
  char thunk_name[50];
  snprintf(thunk_name, sizeof thunk_name, "$thunk%d", thunk_count);
  ++thunk_count;
  return thunk_name;
}

// Return whether a function is a thunk.

bool
Gogo::is_thunk(const Named_object* no)
{
  return no->name().compare(0, 6, "$thunk") == 0;
}

// Define the global names.  We do this only after parsing all the
// input files, because the program might define the global names
// itself.

void
Gogo::define_global_names()
{
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
		error_at(no->location(),
			 "may not define methods for global type");
	      no->set_type_value(global_no->type_value());
	    }
	  else
	    {
	      error_at(no->location(), "expected type");
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
	  error_at(p->second->location(),
		   "%qs defined as both imported name and global name",
		   n.c_str());
	  inform(pf->second, "%qs imported here", n.c_str());
	}

      // No package scope identifier may be named "init".
      if (!p->second->is_function()
	  && Gogo::unpack_hidden_name(p->second->name()) == "init")
	{
	  error_at(p->second->location(),
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
      if (package != this->package_
	  && package->is_imported()
	  && !package->used()
	  && !package->uses_sink_alias()
	  && !quiet)
	error_at(package->location(), "imported and not used: %s",
		 Gogo::message_name(package->package_name()).c_str());
      package->clear_is_imported();
      package->clear_uses_sink_alias();
      package->clear_used();
    }
}

// Queue up a type specific function for later writing.  These are
// written out in write_specific_type_functions, called after the
// parse tree is lowered.

void
Gogo::queue_specific_type_function(Type* type, Named_type* name,
				   const std::string& hash_name,
				   Function_type* hash_fntype,
				   const std::string& equal_name,
				   Function_type* equal_fntype)
{
  go_assert(!this->specific_type_functions_are_written_);
  go_assert(!this->in_global_scope());
  Specific_type_function* tsf = new Specific_type_function(type, name,
							   hash_name,
							   hash_fntype,
							   equal_name,
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
  Named_object* hash_fn;
  Named_object* equal_fn;
  switch (t->classification())
    {
    case Type::TYPE_NAMED:
      {
	Named_type* nt = t->named_type();
	if (!t->compare_is_identity(this->gogo_) && t->is_comparable())
	  t->type_functions(this->gogo_, nt, NULL, NULL, &hash_fn, &equal_fn);

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
      if (!t->compare_is_identity(this->gogo_) && t->is_comparable())
	t->type_functions(this->gogo_, NULL, NULL, NULL, &hash_fn, &equal_fn);
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
      tsf->type->write_specific_type_functions(this, tsf->name,
					       tsf->hash_name,
					       tsf->hash_fntype,
					       tsf->equal_name,
					       tsf->equal_fntype);
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
  return TRAVERSE_SKIP_COMPONENTS;
}

// Lower the parse tree.  This is called after the parse is complete,
// when all names should be resolved.

void
Gogo::lower_parse_tree()
{
  Lower_parse_tree lower_parse_tree(this, NULL);
  this->traverse(&lower_parse_tree);
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

// Create a descriptor for every top-level exported function.

int
Create_function_descriptors::function(Named_object* no)
{
  if (no->is_function()
      && no->func_value()->enclosing() == NULL
      && !no->func_value()->is_method()
      && !Gogo::is_hidden_name(no->name())
      && !Gogo::is_thunk(no))
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

// Look for interface types to finalize methods of inherited
// interfaces.

class Finalize_methods : public Traverse
{
 public:
  Finalize_methods(Gogo* gogo)
    : Traverse(traverse_types),
      gogo_(gogo)
  { }

  int
  type(Type*);

 private:
  Gogo* gogo_;
};

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
	// We have to finalize the methods of the real type first.
	// But if the real type is a struct type, then we only want to
	// finalize the methods of the field types, not of the struct
	// type itself.  We don't want to add methods to the struct,
	// since it has a name.
	Named_type* nt = t->named_type();
	Type* rt = nt->real_type();
	if (rt->classification() != Type::TYPE_STRUCT)
	  {
	    if (Type::traverse(rt, this) == TRAVERSE_EXIT)
	      return TRAVERSE_EXIT;
	  }
	else
	  {
	    if (rt->struct_type()->traverse_field_types(this) == TRAVERSE_EXIT)
	      return TRAVERSE_EXIT;
	  }

	nt->finalize_methods(this->gogo_);

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
	    error_at(var->location(), "incompatible type in initialization");
	  else
	    error_at(var->location(),
		     "incompatible type in initialization (%s)",
		     reason.c_str());
	  var->clear_init();
	}
      else if (!var->is_used()
	       && !var->is_global()
	       && !var->is_parameter()
	       && !var->is_receiver()
	       && !var->type()->is_error()
	       && (init == NULL || !init->is_error_expression())
	       && !Lex::is_invalid_identifier(named_object->name()))
	error_at(var->location(), "%qs declared and not used",
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
	error_at(constant->location(), "const initializer cannot be nil");
      else if (!ctype->is_error())
	error_at(constant->location(), "invalid constant type");
      constant->set_error();
    }
  else if (!constant->expr()->is_constant())
    {
      error_at(constant->expr()->location(), "expression is not constant");
      constant->set_error();
    }
  else if (!Type::are_assignable(constant->type(), constant->expr()->type(),
				 NULL))
    {
      error_at(constant->location(),
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
}

// Check the types in a single block.

void
Gogo::check_types_in_block(Block* block)
{
  Check_types_traverse traverse(this);
  block->traverse(&traverse);
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
Order_eval::statement(Block* block, size_t* pindex, Statement* s)
{
  // FIXME: This approach doesn't work for switch statements, because
  // we add the new statements before the whole switch when we need to
  // instead add them just before the switch expression.  The right
  // fix is probably to lower switch statements with nonconstant cases
  // to a series of conditionals.
  if (s->switch_statement() != NULL)
    return TRAVERSE_CONTINUE;

  Find_eval_ordering find_eval_ordering;

  // If S is a variable declaration, then ordinary traversal won't do
  // anything.  We want to explicitly traverse the initialization
  // expression if there is one.
  Variable_declaration_statement* vds = s->variable_declaration_statement();
  Expression* init = NULL;
  Expression* orig_init = NULL;
  if (vds == NULL)
    s->traverse_contents(&find_eval_ordering);
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
      switch (s->classification())
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
	    Expression* expr = s->expression_statement()->expr();
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

  bool is_thunk = s->thunk_statement() != NULL;
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
	  s = Statement::make_statement(*pexpr, true);
	}

      block->insert_statement_before(*pindex, s);
      ++*pindex;
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

  std::string name = orig_no->name() + "$recover";
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

      const std::string& new_receiver_name(orig_fntype->receiver()->name());
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
// non-nil.  The expression is
// __go_can_recover(__builtin_return_address()).

Expression*
Build_recover_thunks::can_recover_arg(Location location)
{
  static Named_object* builtin_return_address;
  if (builtin_return_address == NULL)
    {
      const Location bloc = Linemap::predeclared_location();

      Typed_identifier_list* param_types = new Typed_identifier_list();
      Type* uint_type = Type::lookup_integer_type("uint");
      param_types->push_back(Typed_identifier("l", uint_type, bloc));

      Typed_identifier_list* return_types = new Typed_identifier_list();
      Type* voidptr_type = Type::make_pointer_type(Type::make_void_type());
      return_types->push_back(Typed_identifier("", voidptr_type, bloc));

      Function_type* fntype = Type::make_function_type(NULL, param_types,
						       return_types, bloc);
      builtin_return_address =
	Named_object::make_function_declaration("__builtin_return_address",
						NULL, fntype, bloc);
      const char* n = "__builtin_return_address";
      builtin_return_address->func_declaration_value()->set_asm_name(n);
    }

  static Named_object* can_recover;
  if (can_recover == NULL)
    {
      const Location bloc = Linemap::predeclared_location();
      Typed_identifier_list* param_types = new Typed_identifier_list();
      Type* voidptr_type = Type::make_pointer_type(Type::make_void_type());
      param_types->push_back(Typed_identifier("a", voidptr_type, bloc));
      Type* boolean_type = Type::lookup_bool_type();
      Typed_identifier_list* results = new Typed_identifier_list();
      results->push_back(Typed_identifier("", boolean_type, bloc));
      Function_type* fntype = Type::make_function_type(NULL, param_types,
						       results, bloc);
      can_recover = Named_object::make_function_declaration("__go_can_recover",
							    NULL, fntype,
							    bloc);
      can_recover->func_declaration_value()->set_asm_name("__go_can_recover");
    }

  Expression* fn = Expression::make_func_reference(builtin_return_address,
						   NULL, location);

  mpz_t zval;
  mpz_init_set_ui(zval, 0UL);
  Expression* zexpr = Expression::make_integer(&zval, NULL, location);
  mpz_clear(zval);
  Expression_list *args = new Expression_list();
  args->push_back(zexpr);

  Expression* call = Expression::make_call(fn, args, false, location);

  args = new Expression_list();
  args->push_back(call);

  fn = Expression::make_func_reference(can_recover, NULL, location);
  return Expression::make_call(fn, args, false, location);
}

// Build thunks for functions which call recover.  We build a new
// function with an extra parameter, which is whether a call to
// recover can succeed.  We then move the body of this function to
// that one.  We then turn this function into a thunk which calls the
// new one, passing the value of
// __go_can_recover(__builtin_return_address()).  The function will be
// marked as not splitting the stack.  This will cooperate with the
// implementation of defer to make recover do the right thing.

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
		  nt->interface_method_table(this->gogo_, *p, false);
		  nt->interface_method_table(this->gogo_, *p, true);
		}
	    }
	  else
	    {
	      if ((*p)->implements_interface(Type::make_pointer_type(st),
					     NULL))
		{
		  st->interface_method_table(this->gogo_, *p, false);
		  st->interface_method_table(this->gogo_, *p, true);
		}
	    }
	}
    }
  return TRAVERSE_CONTINUE;
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
    error_at(func->block()->end_location(),
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

// Work out the package priority.  It is one more than the maximum
// priority of an imported package.

int
Gogo::package_priority() const
{
  int priority = 0;
  for (Packages::const_iterator p = this->packages_.begin();
       p != this->packages_.end();
       ++p)
    if (p->second->priority() > priority)
      priority = p->second->priority();
  return priority + 1;
}

// Export identifiers as requested.

void
Gogo::do_exports()
{
  // For now we always stream to a section.  Later we may want to
  // support streaming to a separate file.
  Stream_to_section stream;

  Export exp(&stream);
  exp.register_builtin_types(this);
  exp.export_globals(this->package_name(),
		     this->pkgpath(),
		     this->package_priority(),
		     this->imports_,
		     (this->need_init_fn_ && !this->is_main_package()
		      ? this->get_init_fn_name()
		      : ""),
		     this->imported_init_fns_,
		     this->package_->bindings());
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
  Map_type::make_map_descriptor_type();
  Channel_type::make_chan_type_descriptor_type();
  Interface_type::make_interface_type_descriptor_type();
  Expression::make_func_descriptor_type();
  Type::convert_builtin_named_types(this);

  Runtime::convert_types(this);

  this->named_types_are_converted_ = true;
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

// Class Function.

Function::Function(Function_type* type, Function* enclosing, Block* block,
		   Location location)
  : type_(type), enclosing_(enclosing), results_(NULL),
    closure_var_(NULL), block_(block), location_(location), labels_(),
    local_type_count_(0), descriptor_(NULL), fndecl_(NULL), defer_stack_(NULL),
    is_sink_(false), results_are_named_(false), nointerface_(false),
    calls_recover_(false), is_recover_thunk_(false), has_recover_thunk_(false),
    in_unique_section_(false)
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
      Type* struct_type = Type::make_struct_type(sfl, loc);
      Variable* var = new Variable(Type::make_pointer_type(struct_type),
				   NULL, false, false, false, loc);
      var->set_is_used();
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
  st->push_field(Struct_field(Typed_identifier(".$f", voidptr_type,
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
  if (ins.second)
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
	  error_at(location, "label %qs already defined",
		   Gogo::message_name(label_name).c_str());
	  inform(label->location(), "previous definition of %qs was here",
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
	error_at(label->location(), "label %qs defined and not used",
		 Gogo::message_name(label->name()).c_str());
    }
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
Function::export_func(Export* exp, const std::string& name) const
{
  Function::export_func_with_type(exp, name, this->type_);
}

// Export a function with a type.

void
Function::export_func_with_type(Export* exp, const std::string& name,
				const Function_type* fntype)
{
  exp->write_c_string("func ");

  if (fntype->is_method())
    {
      exp->write_c_string("(");
      const Typed_identifier* receiver = fntype->receiver();
      exp->write_name(receiver->name());
      exp->write_c_string(" ");
      exp->write_type(receiver->type());
      exp->write_c_string(") ");
    }

  exp->write_string(name);

  exp->write_c_string(" (");
  const Typed_identifier_list* parameters = fntype->parameters();
  if (parameters != NULL)
    {
      bool is_varargs = fntype->is_varargs();
      bool first = true;
      for (Typed_identifier_list::const_iterator p = parameters->begin();
	   p != parameters->end();
	   ++p)
	{
	  if (first)
	    first = false;
	  else
	    exp->write_c_string(", ");
	  exp->write_name(p->name());
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

  const Typed_identifier_list* results = fntype->results();
  if (results != NULL)
    {
      if (results->size() == 1 && results->begin()->name().empty())
	{
	  exp->write_c_string(" ");
	  exp->write_type(results->begin()->type());
	}
      else
	{
	  exp->write_c_string(" (");
	  bool first = true;
	  for (Typed_identifier_list::const_iterator p = results->begin();
	       p != results->end();
	       ++p)
	    {
	      if (first)
		first = false;
	      else
		exp->write_c_string(", ");
	      exp->write_name(p->name());
	      exp->write_c_string(" ");
	      exp->write_type(p->type());
	    }
	  exp->write_c_string(")");
	}
    }
  exp->write_c_string(";\n");
}

// Import a function.

void
Function::import_func(Import* imp, std::string* pname,
		      Typed_identifier** preceiver,
		      Typed_identifier_list** pparameters,
		      Typed_identifier_list** presults,
		      bool* is_varargs)
{
  imp->require_c_string("func ");

  *preceiver = NULL;
  if (imp->peek_char() == '(')
    {
      imp->require_c_string("(");
      std::string name = imp->read_name();
      imp->require_c_string(" ");
      Type* rtype = imp->read_type();
      *preceiver = new Typed_identifier(name, rtype, imp->location());
      imp->require_c_string(") ");
    }

  *pname = imp->read_identifier();

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
	  imp->require_c_string(" ");

	  if (imp->match_c_string("..."))
	    {
	      imp->advance(3);
	      *is_varargs = true;
	    }

	  Type* ptype = imp->read_type();
	  if (*is_varargs)
	    ptype = Type::make_array_type(ptype, NULL);
	  parameters->push_back(Typed_identifier(name, ptype,
						 imp->location()));
	  if (imp->peek_char() != ',')
	    break;
	  go_assert(!*is_varargs);
	  imp->require_c_string(", ");
	}
    }
  imp->require_c_string(")");
  *pparameters = parameters;

  Typed_identifier_list* results;
  if (imp->peek_char() != ' ')
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
	      imp->require_c_string(" ");
	      Type* rtype = imp->read_type();
	      results->push_back(Typed_identifier(name, rtype,
						  imp->location()));
	      if (imp->peek_char() != ',')
		break;
	      imp->require_c_string(", ");
	    }
	  imp->require_c_string(")");
	}
    }
  imp->require_c_string(";\n");
  *presults = results;
}

// Class Block.

Block::Block(Block* enclosing, Location location)
  : enclosing_(enclosing), statements_(),
    bindings_(new Bindings(enclosing == NULL
			   ? NULL
			   : enclosing->bindings())),
    start_location_(location),
    end_location_(UNKNOWN_LOCATION)
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
      const unsigned int e_or_t = (Traverse::traverse_expressions
				   | Traverse::traverse_types);
      const unsigned int e_or_t_or_s = (e_or_t
					| Traverse::traverse_statements);
      for (Bindings::const_definitions_iterator pb =
	     this->bindings_->begin_definitions();
	   pb != this->bindings_->end_definitions();
	   ++pb)
	{
	  int t = TRAVERSE_CONTINUE;
	  switch ((*pb)->classification())
	    {
	    case Named_object::NAMED_OBJECT_CONST:
	      if ((traverse_mask & Traverse::traverse_constants) != 0)
		t = traverse->constant(*pb, false);
	      if (t == TRAVERSE_CONTINUE
		  && (traverse_mask & e_or_t) != 0)
		{
		  Type* tc = (*pb)->const_value()->type();
		  if (tc != NULL
		      && Type::traverse(tc, traverse) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		  t = (*pb)->const_value()->traverse_expression(traverse);
		}
	      break;

	    case Named_object::NAMED_OBJECT_VAR:
	    case Named_object::NAMED_OBJECT_RESULT_VAR:
	      if ((traverse_mask & Traverse::traverse_variables) != 0)
		t = traverse->variable(*pb);
	      if (t == TRAVERSE_CONTINUE
		  && (traverse_mask & e_or_t) != 0)
		{
		  if ((*pb)->is_result_variable()
		      || (*pb)->var_value()->has_type())
		    {
		      Type* tv = ((*pb)->is_variable()
				  ? (*pb)->var_value()->type()
				  : (*pb)->result_var_value()->type());
		      if (tv != NULL
			  && Type::traverse(tv, traverse) == TRAVERSE_EXIT)
			return TRAVERSE_EXIT;
		    }
		}
	      if (t == TRAVERSE_CONTINUE
		  && (traverse_mask & e_or_t_or_s) != 0
		  && (*pb)->is_variable())
		t = (*pb)->var_value()->traverse_expression(traverse,
							    traverse_mask);
	      break;

	    case Named_object::NAMED_OBJECT_FUNC:
	    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
	      go_unreachable();

	    case Named_object::NAMED_OBJECT_TYPE:
	      if ((traverse_mask & e_or_t) != 0)
		t = Type::traverse((*pb)->type_value(), traverse);
	      break;

	    case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
	    case Named_object::NAMED_OBJECT_UNKNOWN:
	    case Named_object::NAMED_OBJECT_ERRONEOUS:
	      break;

	    case Named_object::NAMED_OBJECT_PACKAGE:
	    case Named_object::NAMED_OBJECT_SINK:
	      go_unreachable();

	    default:
	      go_unreachable();
	    }

	  if (t == TRAVERSE_EXIT)
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

  // FIXME: Permitting FUNCTION to be NULL here is a temporary measure
  // until we have a proper representation of the init function.
  Bfunction* bfunction;
  if (function == NULL)
    bfunction = NULL;
  else
    bfunction = tree_to_function(function->func_value()->get_decl());
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
	  error_at(loc, "goto jumps into block");
	  inform(bto->start_location(), "goto target block starts here");
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

      std::string n = (*p)->message_name();
      error_at(loc, "goto jumps over declaration of %qs", n.c_str());
      inform((*p)->location(), "%qs defined here", n.c_str());
    }
}

// Class Function_declaration.

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
    backend_(NULL), is_global_(is_global), is_parameter_(is_parameter),
    is_receiver_(is_receiver), is_varargs_parameter_(false), is_used_(false),
    is_address_taken_(false), is_non_escaping_address_taken_(false),
    seen_(false), init_is_lowered_(false), type_from_init_tuple_(false),
    type_from_range_index_(false), type_from_range_value_(false),
    type_from_chan_element_(false), is_type_switch_var_(false),
    determined_type_(false), in_unique_section_(false)
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
	error_at(this->location(), "invalid tuple definition");
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
	    error_at(this->location(),
		     "invalid definition of value variable for channel range");
	  return Type::make_error_type();
	}
    }
  else
    {
      if (report_error)
	error_at(this->location(), "invalid type for range clause");
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
	error_at(this->location(), "expected channel");
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
	  error_at(this->location_, "variable initializer refers to itself");
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
  if (this->is_type_switch_var_ && this->type_->is_nil_constant_as_type())
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
	      error_at(this->location_, "variable has no type");
	      type = Type::make_error_type();
	    }
	  else if (type->is_nil_type())
	    {
	      error_at(this->location_, "variable defined to nil type");
	      type = Type::make_error_type();
	    }
	  else if (type->is_call_multiple_result_type())
	    {
	      error_at(this->location_,
		       "single variable set to multiple-value function call");
	      type = Type::make_error_type();
	    }

	  this->type_ = type;
	}
    }
}

// Export the variable

void
Variable::export_var(Export* exp, const std::string& name) const
{
  go_assert(this->is_global_);
  exp->write_c_string("var ");
  exp->write_string(name);
  exp->write_c_string(" ");
  exp->write_type(this->type());
  exp->write_c_string(";\n");
}

// Import a variable.

void
Variable::import_var(Import* imp, std::string* pname, Type** ptype)
{
  imp->require_c_string("var ");
  *pname = imp->read_identifier();
  imp->require_c_string(" ");
  *ptype = imp->read_type();
  imp->require_c_string(";\n");
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

	  std::string n = Gogo::unpack_hidden_name(name);
	  Btype* btype = type->get_backend(gogo);

	  Bvariable* bvar;
	  if (this->is_global_)
	    bvar = backend->global_variable((package == NULL
					     ? gogo->package_name()
					     : package->package_name()),
					    (package == NULL
					     ? gogo->pkgpath_symbol()
					     : package->pkgpath_symbol()),
					    n,
					    btype,
					    package != NULL,
					    Gogo::is_hidden_name(name),
					    this->in_unique_section_,
					    this->location_);
	  else if (function == NULL)
	    {
	      go_assert(saw_errors());
	      bvar = backend->error_variable();
	    }
	  else
	    {
	      tree fndecl = function->func_value()->get_decl();
	      Bfunction* bfunction = tree_to_function(fndecl);
	      bool is_address_taken = (this->is_non_escaping_address_taken_
				       && !this->is_in_heap());
	      if (is_parameter)
		bvar = backend->parameter_variable(bfunction, n, btype,
						   is_address_taken,
						   this->location_);
	      else
		bvar = backend->local_variable(bfunction, n, btype,
					       is_address_taken,
					       this->location_);
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
	  tree fndecl = function->func_value()->get_decl();
	  Bfunction* bfunction = tree_to_function(fndecl);
	  std::string n = Gogo::unpack_hidden_name(name);
	  bool is_address_taken = (this->is_non_escaping_address_taken_
				   && !this->is_in_heap());
	  this->backend_ = backend->local_variable(bfunction, n, btype,
						   is_address_taken,
						   this->location_);
	}
    }
  return this->backend_;
}

// Class Named_constant.

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
  this->expr()->export_expression(exp);
  exp->write_c_string(";\n");
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
  *pexpr = Expression::import_expression(imp);
  imp->require_c_string(";\n");
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

// Return whether any methods ere defined.

bool
Type_declaration::has_methods() const
{
  return !this->methods_.empty();
}

// Define methods for the real type.

void
Type_declaration::define_methods(Named_type* nt)
{
  for (std::vector<Named_object*>::const_iterator p = this->methods_.begin();
       p != this->methods_.end();
       ++p)
    nt->add_existing_method(*p);
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
    tree_(NULL)
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
      this->type_value()->export_named_type(exp, this->name_);
      break;

    case NAMED_OBJECT_TYPE_DECLARATION:
      error_at(this->type_declaration_value()->location(),
	       "attempt to export %<%s%> which was declared but not defined",
	       this->message_name().c_str());
      break;

    case NAMED_OBJECT_FUNC_DECLARATION:
      this->func_declaration_value()->export_func(exp, this->name_);
      break;

    case NAMED_OBJECT_VAR:
      this->var_value()->export_var(exp, this->name_);
      break;

    case NAMED_OBJECT_RESULT_VAR:
    case NAMED_OBJECT_SINK:
      go_unreachable();

    case NAMED_OBJECT_FUNC:
      this->func_value()->export_func(exp, this->name_);
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
      if (new_object->is_function_declaration())
	{
	  if (!new_object->func_declaration_value()->asm_name().empty())
	    sorry("__asm__ for function definitions");
	  Function_type* old_type = old_object->func_value()->type();
	  Function_type* new_type =
	    new_object->func_declaration_value()->type();
	  if (old_type->is_valid_redeclaration(new_type, &reason))
	    return old_object;
	}
      break;

    case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
      {
	Function_type* old_type = old_object->func_declaration_value()->type();
	if (new_object->is_function_declaration())
	  {
	    Function_type* new_type =
	      new_object->func_declaration_value()->type();
	    if (old_type->is_valid_redeclaration(new_type, &reason))
	      return old_object;
	  }
	if (new_object->is_function())
	  {
	    Function_type* new_type = new_object->func_value()->type();
	    if (old_type->is_valid_redeclaration(new_type, &reason))
	      {
		if (!old_object->func_declaration_value()->asm_name().empty())
		  sorry("__asm__ for function definitions");
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
    error_at(new_object->location(), "redefinition of %qs", n.c_str());
  else
    error_at(new_object->location(), "redefinition of %qs: %s", n.c_str(),
	     reason.c_str());

  inform(old_object->location(), "previous definition of %qs was here",
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
  const unsigned int e_or_t_or_s = (e_or_t
				    | Traverse::traverse_statements);
  for (size_t i = 0; i < this->named_objects_.size(); ++i)
    {
      Named_object* p = this->named_objects_[i];
      int t = TRAVERSE_CONTINUE;
      switch (p->classification())
	{
	case Named_object::NAMED_OBJECT_CONST:
	  if ((traverse_mask & Traverse::traverse_constants) != 0)
	    t = traverse->constant(p, is_global);
	  if (t == TRAVERSE_CONTINUE
	      && (traverse_mask & e_or_t) != 0)
	    {
	      Type* tc = p->const_value()->type();
	      if (tc != NULL
		  && Type::traverse(tc, traverse) == TRAVERSE_EXIT)
		return TRAVERSE_EXIT;
	      t = p->const_value()->traverse_expression(traverse);
	    }
	  break;

	case Named_object::NAMED_OBJECT_VAR:
	case Named_object::NAMED_OBJECT_RESULT_VAR:
	  if ((traverse_mask & Traverse::traverse_variables) != 0)
	    t = traverse->variable(p);
	  if (t == TRAVERSE_CONTINUE
	      && (traverse_mask & e_or_t) != 0)
	    {
	      if (p->is_result_variable()
		  || p->var_value()->has_type())
		{
		  Type* tv = (p->is_variable()
			      ? p->var_value()->type()
			      : p->result_var_value()->type());
		  if (tv != NULL
		      && Type::traverse(tv, traverse) == TRAVERSE_EXIT)
		    return TRAVERSE_EXIT;
		}
	    }
	  if (t == TRAVERSE_CONTINUE
	      && (traverse_mask & e_or_t_or_s) != 0
	      && p->is_variable())
	    t = p->var_value()->traverse_expression(traverse, traverse_mask);
	  break;

	case Named_object::NAMED_OBJECT_FUNC:
	  if ((traverse_mask & Traverse::traverse_functions) != 0)
	    t = traverse->function(p);

	  if (t == TRAVERSE_CONTINUE
	      && (traverse_mask
		  & (Traverse::traverse_variables
		     | Traverse::traverse_constants
		     | Traverse::traverse_functions
		     | Traverse::traverse_blocks
		     | Traverse::traverse_statements
		     | Traverse::traverse_expressions
		     | Traverse::traverse_types)) != 0)
	    t = p->func_value()->traverse(traverse);
	  break;

	case Named_object::NAMED_OBJECT_PACKAGE:
	  // These are traversed in Gogo::traverse.
	  go_assert(is_global);
	  break;

	case Named_object::NAMED_OBJECT_TYPE:
	  if ((traverse_mask & e_or_t) != 0)
	    t = Type::traverse(p->type_value(), traverse);
	  break;

	case Named_object::NAMED_OBJECT_TYPE_DECLARATION:
	case Named_object::NAMED_OBJECT_FUNC_DECLARATION:
	case Named_object::NAMED_OBJECT_UNKNOWN:
	case Named_object::NAMED_OBJECT_ERRONEOUS:
	  break;

	case Named_object::NAMED_OBJECT_SINK:
	default:
	  go_unreachable();
	}

      if (t == TRAVERSE_EXIT)
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

  return TRAVERSE_CONTINUE;
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
      tree fndecl = function->get_decl();
      Bfunction* bfunction = tree_to_function(fndecl);
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

// Class Unnamed_label.

// Get the backend representation for an unnamed label.

Blabel*
Unnamed_label::get_blabel(Translate_context* context)
{
  if (this->blabel_ == NULL)
    {
      Function* function = context->function()->func_value();
      tree fndecl = function->get_decl();
      Bfunction* bfunction = tree_to_function(fndecl);
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

Package::Package(const std::string& pkgpath, Location location)
  : pkgpath_(pkgpath), pkgpath_symbol_(Gogo::pkgpath_for_symbol(pkgpath)),
    package_name_(), bindings_(new Bindings(NULL)), priority_(0),
    location_(location), used_(false), is_imported_(false),
    uses_sink_alias_(false)
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
    error_at(location,
	     "saw two different packages with the same package path %s: %s, %s",
	     this->pkgpath_.c_str(), this->package_name_.c_str(),
	     package_name.c_str());
}

// Set the priority.  We may see multiple priorities for an imported
// package; we want to use the largest one.

void
Package::set_priority(int priority)
{
  if (priority > this->priority_)
    this->priority_ = priority;
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
  // type I interface { F() interface{I} }
  if (type->classification() != Type::TYPE_NAMED
      && type->classification() != Type::TYPE_INTERFACE)
    return false;
  if (this->types_seen_ == NULL)
    this->types_seen_ = new Types_seen();
  std::pair<Types_seen::iterator, bool> ins = this->types_seen_->insert(type);
  return !ins.second;
}

// Record that we are looking at an expression, and return true if we
// have already seen it.

bool
Traverse::remember_expression(const Expression* expression)
{
  go_assert((this->traverse_mask() & traverse_types) != 0
	     || (this->traverse_mask() & traverse_expressions) != 0);
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

// Class Statement_inserter.

void
Statement_inserter::insert(Statement* s)
{
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
