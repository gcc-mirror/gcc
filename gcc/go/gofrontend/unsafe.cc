// unsafe.cc -- Go frontend builtin unsafe package.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "go-c.h"
#include "types.h"
#include "gogo.h"

// Set up the builtin unsafe package.  This should probably be driven
// by a table.

void
Gogo::import_unsafe(const std::string& local_name, bool is_local_name_exported,
		    source_location location)
{
  location_t bloc = BUILTINS_LOCATION;

  bool add_to_globals;
  Package* package = this->add_imported_package("unsafe", local_name,
						is_local_name_exported,
						"libgo_unsafe",
						location, &add_to_globals);

  if (package == NULL)
    {
      go_assert(saw_errors());
      return;
    }

  package->set_location(location);
  package->set_is_imported();

  Bindings* bindings = package->bindings();

  // The type may have already been created by an import.
  Named_object* no = package->bindings()->lookup("Pointer");
  if (no == NULL)
    {
      Type* type = Type::make_pointer_type(Type::make_void_type());
      no = bindings->add_type("Pointer", package, type, UNKNOWN_LOCATION);
    }
  else
    {
      go_assert(no->package() == package);
      go_assert(no->is_type());
      go_assert(no->type_value()->is_unsafe_pointer_type());
      no->type_value()->set_is_visible();
    }
  Named_type* pointer_type = no->type_value();
  if (add_to_globals)
    this->add_named_type(pointer_type);

  Type* int_type = this->lookup_global("int")->type_value();

  // Sizeof.
  Typed_identifier_list* results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", int_type, bloc));
  Function_type* fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_builtin();
  no = bindings->add_function_declaration("Sizeof", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // Offsetof.
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", int_type, bloc));
  fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_varargs();
  fntype->set_is_builtin();
  no = bindings->add_function_declaration("Offsetof", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // Alignof.
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", int_type, bloc));
  fntype = Type::make_function_type(NULL, NULL, results, bloc);
  fntype->set_is_varargs();
  fntype->set_is_builtin();
  no = bindings->add_function_declaration("Alignof", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // Typeof.
  Type* empty_interface = Type::make_interface_type(NULL, bloc);
  Typed_identifier_list* parameters = new Typed_identifier_list;
  parameters->push_back(Typed_identifier("i", empty_interface, bloc));
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", empty_interface, bloc));
  fntype = Type::make_function_type(NULL, parameters, results, bloc);
  no = bindings->add_function_declaration("Typeof", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // Reflect.
  parameters = new Typed_identifier_list;
  parameters->push_back(Typed_identifier("it", empty_interface, bloc));
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", empty_interface, bloc));
  results->push_back(Typed_identifier("", pointer_type, bloc));
  fntype = Type::make_function_type(NULL, parameters, results, bloc);
  no = bindings->add_function_declaration("Reflect", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // Unreflect.
  parameters = new Typed_identifier_list;
  parameters->push_back(Typed_identifier("typ", empty_interface, bloc));
  parameters->push_back(Typed_identifier("addr", pointer_type, bloc));
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", empty_interface, bloc));
  fntype = Type::make_function_type(NULL, parameters, results, bloc);
  no = bindings->add_function_declaration("Unreflect", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // New.
  parameters = new Typed_identifier_list;
  parameters->push_back(Typed_identifier("typ", empty_interface, bloc));
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", pointer_type, bloc));
  fntype = Type::make_function_type(NULL, parameters, results, bloc);
  no = bindings->add_function_declaration("New", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  // NewArray.
  parameters = new Typed_identifier_list;
  parameters->push_back(Typed_identifier("typ", empty_interface, bloc));
  parameters->push_back(Typed_identifier("n", int_type, bloc));
  results = new Typed_identifier_list;
  results->push_back(Typed_identifier("", pointer_type, bloc));
  fntype = Type::make_function_type(NULL, parameters, results, bloc);
  no = bindings->add_function_declaration("NewArray", package, fntype, bloc);
  if (add_to_globals)
    this->add_named_object(no);

  if (!this->imported_unsafe_)
    {
      go_imported_unsafe();
      this->imported_unsafe_ = true;
    }
}
