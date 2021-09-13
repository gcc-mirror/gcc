// unsafe.cc -- Go frontend builtin unsafe package.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "types.h"
#include "gogo.h"

// Set up the builtin unsafe package.

void
Gogo::import_unsafe(const std::string& local_name, bool is_local_name_exported,
		    Location location)
{
  bool add_to_globals;
  Package* package = this->add_imported_package("unsafe", local_name,
						is_local_name_exported,
						"unsafe", "unsafe", location,
						&add_to_globals);

  if (package == NULL)
    {
      go_assert(saw_errors());
      return;
    }

  package->set_location(location);
  this->imports_.insert(std::make_pair("unsafe", package));

  this->add_unsafe_bindings(package);

  Named_object* pointer_no = package->bindings()->lookup_local("Pointer");
  pointer_no->type_value()->set_is_visible();

  if (add_to_globals)
    {
      Bindings* bindings = package->bindings();
      for (Bindings::const_declarations_iterator p =
	     bindings->begin_declarations();
	   p != bindings->end_declarations();
	   ++p)
	this->add_dot_import_object(p->second);
    }
}

// Add the unsafe bindings to the Package object.  This should
// probably be driven by a table.

void
Gogo::add_unsafe_bindings(Package* package)
{
  Bindings* bindings = package->bindings();

  if (bindings->lookup_local("Sizeof") != NULL)
    {
      // Already done by an earlier import.
      return;
    }

  Location bloc = Linemap::predeclared_location();

  // The type may have already been created by an import.
  Named_object* no = bindings->lookup("Pointer");
  if (no == NULL)
    {
      Type* type = Type::make_pointer_type(Type::make_void_type());
      no = bindings->add_type("Pointer", package, type,
			      Linemap::unknown_location());
    }
  else
    {
      go_assert(no->package() == package);
      go_assert(no->is_type());
      go_assert(no->type_value()->is_unsafe_pointer_type());
    }
  Named_type* pointer_type = no->type_value();

  // This may be called during an import, so the type may not be
  // visible yet.
  pointer_type->clear_is_visible();

  Type* uintptr_type = Type::lookup_integer_type("uintptr");

  // Sizeof.
  Typed_identifier_list* results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", uintptr_type, bloc));
  Function_type* fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_builtin();
  bindings->add_function_declaration("Sizeof", package, fntype, bloc);

  // Offsetof.
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", uintptr_type, bloc));
  fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_varargs();
  fntype->set_is_builtin();
  bindings->add_function_declaration("Offsetof", package, fntype, bloc);

  // Alignof.
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", uintptr_type, bloc));
  fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_varargs();
  fntype->set_is_builtin();
  bindings->add_function_declaration("Alignof", package, fntype, bloc);

  // Add.
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", pointer_type, bloc));
  fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_builtin();
  bindings->add_function_declaration("Add", package, fntype, bloc);

  // Slice.
  fntype = Type::make_function_type(NULL, NULL, NULL, bloc);
  fntype->set_is_builtin();
  bindings->add_function_declaration("Slice", package, fntype, bloc);

  if (!this->imported_unsafe_)
    {
      go_imported_unsafe();
      this->imported_unsafe_ = true;
    }
}
