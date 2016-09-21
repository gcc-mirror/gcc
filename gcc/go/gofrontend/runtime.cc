// runtime.cc -- runtime functions called by generated code

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "gogo.h"
#include "types.h"
#include "expressions.h"
#include "runtime.h"

// The frontend generates calls to various runtime functions.  They
// are implemented in libgo/runtime.  This is how the runtime
// functions are represented in the frontend.  Note that there is
// currently nothing which ensures that the compiler's understanding
// of the runtime function matches the actual implementation in
// libgo/runtime.

// Parameter and result types used by runtime functions.

enum Runtime_function_type
{
  // General indicator that value is not used.
  RFT_VOID,
  // Go untyped bool, C type _Bool.
  RFT_BOOL,
  // Go type *bool, C type _Bool*.
  RFT_BOOLPTR,
  // Go type int, C type intgo.
  RFT_INT,
  // Go type int32, C type int32_t.
  RFT_INT32,
  // Go type int64, C type int64_t.
  RFT_INT64,
  // Go type uint64, C type uint64_t.
  RFT_UINT64,
  // Go type uintptr, C type uintptr_t.
  RFT_UINTPTR,
  // Go type rune, C type int32_t.
  RFT_RUNE,
  // Go type float64, C type double.
  RFT_FLOAT64,
  // Go type complex64, C type __complex float.
  RFT_COMPLEX64,
  // Go type complex128, C type __complex double.
  RFT_COMPLEX128,
  // Go type string, C type struct __go_string.
  RFT_STRING,
  // Go type unsafe.Pointer, C type "void *".
  RFT_POINTER,
  // Go type []any, C type struct __go_open_array.
  RFT_SLICE,
  // Go type map[any]any, C type struct __go_map *.
  RFT_MAP,
  // Go type chan any, C type struct __go_channel *.
  RFT_CHAN,
  // Go type non-empty interface, C type struct __go_interface.
  RFT_IFACE,
  // Go type interface{}, C type struct __go_empty_interface.
  RFT_EFACE,
  // Go type func(unsafe.Pointer), C type void (*) (void *).
  RFT_FUNC_PTR,
  // Pointer to Go type descriptor.
  RFT_TYPE,

  NUMBER_OF_RUNTIME_FUNCTION_TYPES
};

// The Type structures for the runtime function types.

static Type* runtime_function_types[NUMBER_OF_RUNTIME_FUNCTION_TYPES];

// Get the Type for a Runtime_function_type code.

static Type*
runtime_function_type(Runtime_function_type bft)
{
  go_assert(bft < NUMBER_OF_RUNTIME_FUNCTION_TYPES);
  Type* any = Type::make_pointer_type(Type::make_void_type());
  if (runtime_function_types[bft] == NULL)
    {
      const Location bloc = Linemap::predeclared_location();
      Type* t;
      switch (bft)
	{
	default:
	case RFT_VOID:
	  go_unreachable();

	case RFT_BOOL:
	  t = Type::make_boolean_type();
	  break;

	case RFT_BOOLPTR:
	  t = Type::make_pointer_type(Type::lookup_bool_type());
	  break;

	case RFT_INT:
	  t = Type::lookup_integer_type("int");
	  break;

	case RFT_INT32:
	  t = Type::lookup_integer_type("int32");
	  break;

	case RFT_INT64:
	  t = Type::lookup_integer_type("int64");
	  break;

	case RFT_UINT64:
	  t = Type::lookup_integer_type("uint64");
	  break;

	case RFT_RUNE:
	  t = Type::lookup_integer_type("int32");
	  break;

	case RFT_UINTPTR:
	  t = Type::lookup_integer_type("uintptr");
	  break;

	case RFT_FLOAT64:
	  t = Type::lookup_float_type("float64");
	  break;

	case RFT_COMPLEX64:
	  t = Type::lookup_complex_type("complex64");
	  break;

	case RFT_COMPLEX128:
	  t = Type::lookup_complex_type("complex128");
	  break;

	case RFT_STRING:
	  t = Type::lookup_string_type();
	  break;

	case RFT_POINTER:
	  t = Type::make_pointer_type(Type::make_void_type());
	  break;

	case RFT_SLICE:
	  t = Type::make_array_type(any, NULL);
	  break;

	case RFT_MAP:
	  t = Type::make_map_type(any, any, bloc);
	  break;

	case RFT_CHAN:
	  t = Type::make_channel_type(true, true, any);
	  break;

	case RFT_IFACE:
	  {
	    Typed_identifier_list* methods = new Typed_identifier_list();
	    Type* mtype = Type::make_function_type(NULL, NULL, NULL, bloc);
	    methods->push_back(Typed_identifier("x", mtype, bloc));
	    Interface_type* it = Type::make_interface_type(methods, bloc);
	    it->finalize_methods();
	    t = it;
	  }
	  break;

	case RFT_EFACE:
	  t = Type::make_empty_interface_type(bloc);
	  break;

	case RFT_FUNC_PTR:
	  {
	    Typed_identifier_list* param_types = new Typed_identifier_list();
	    Type* ptrtype = runtime_function_type(RFT_POINTER);
	    param_types->push_back(Typed_identifier("", ptrtype, bloc));
	    t = Type::make_function_type(NULL, param_types, NULL, bloc);
	  }
	  break;

	case RFT_TYPE:
	  t = Type::make_type_descriptor_ptr_type();
	  break;
	}

      runtime_function_types[bft] = t;
    }

  return runtime_function_types[bft];
}

// Convert an expression to the type to pass to a runtime function.

static Expression*
convert_to_runtime_function_type(Runtime_function_type bft, Expression* e,
				 Location loc)
{
  switch (bft)
    {
    default:
    case RFT_VOID:
      go_unreachable();

    case RFT_BOOL:
    case RFT_BOOLPTR:
    case RFT_INT:
    case RFT_INT32:
    case RFT_INT64:
    case RFT_UINT64:
    case RFT_UINTPTR:
    case RFT_RUNE:
    case RFT_FLOAT64:
    case RFT_COMPLEX64:
    case RFT_COMPLEX128:
    case RFT_STRING:
    case RFT_POINTER:
    case RFT_FUNC_PTR:
      {
	Type* t = runtime_function_type(bft);
	if (!Type::are_identical(t, e->type(), true, NULL))
	  e = Expression::make_cast(t, e, loc);
	return e;
      }

    case RFT_SLICE:
    case RFT_MAP:
    case RFT_CHAN:
    case RFT_IFACE:
    case RFT_EFACE:
      return Expression::make_unsafe_cast(runtime_function_type(bft), e, loc);

    case RFT_TYPE:
      go_assert(e->type() == Type::make_type_descriptor_ptr_type());
      return e;
    }
}

// Convert all the types used for runtime functions to the backend
// representation.

void
Runtime::convert_types(Gogo* gogo)
{
  for (int i = 0; i < static_cast<int>(NUMBER_OF_RUNTIME_FUNCTION_TYPES); ++i)
    {
      Type* t = runtime_function_types[i];
      if (t != NULL && t->named_type() != NULL)
	{
	  bool r = t->verify();
	  go_assert(r);
	  t->named_type()->convert(gogo);
	}
    }
}

// The type used to define a runtime function.

struct Runtime_function
{
  // Function name.
  const char* name;
  // Parameter types.  Never more than 6, as it happens.  RFT_VOID if
  // not used.
  Runtime_function_type parameter_types[6];
  // Result types.  Never more than 2, as it happens.  RFT_VOID if not
  // used.
  Runtime_function_type result_types[2];
};

static const Runtime_function runtime_functions[] =
{

#define DEF_GO_RUNTIME(CODE, NAME, PARAMS, RESULTS) { NAME, PARAMS, RESULTS } ,

#include "runtime.def"

#undef DEF_GO_RUNTIME

};

static Named_object*
runtime_function_declarations[Runtime::NUMBER_OF_FUNCTIONS];

// Get the declaration of a runtime function.

Named_object*
Runtime::runtime_declaration(Function code)
{
  go_assert(code < Runtime::NUMBER_OF_FUNCTIONS);
  if (runtime_function_declarations[code] == NULL)
    {
      const Runtime_function* pb = &runtime_functions[code];

      Location bloc = Linemap::predeclared_location();

      Typed_identifier_list* param_types = NULL;
      if (pb->parameter_types[0] != RFT_VOID)
	{
	  param_types = new Typed_identifier_list();
	  for (unsigned int i = 0;
	       i < (sizeof(pb->parameter_types)
		    / sizeof (pb->parameter_types[0]));
	       i++)
	    {
	      if (pb->parameter_types[i] == RFT_VOID)
		break;
	      Type* t = runtime_function_type(pb->parameter_types[i]);
	      param_types->push_back(Typed_identifier("", t, bloc));
	    }
	}

      Typed_identifier_list* result_types = NULL;
      if (pb->result_types[0] != RFT_VOID)
	{
	  result_types = new Typed_identifier_list();
	  for (unsigned int i = 0;
	       i < sizeof(pb->result_types) / sizeof(pb->result_types[0]);
	       i++)
	    {
	      if (pb->result_types[i] == RFT_VOID)
		break;
	      Type* t = runtime_function_type(pb->result_types[i]);
	      result_types->push_back(Typed_identifier("", t, bloc));
	    }
	}

      Function_type* fntype = Type::make_function_type(NULL, param_types,
						       result_types, bloc);
      const char* n = pb->name;
      const char* n1 = strchr(n, '.');
      if (n1 != NULL)
	n = n1 + 1;
      Named_object* no = Named_object::make_function_declaration(n, NULL,
								 fntype, bloc);
      no->func_declaration_value()->set_asm_name(pb->name);

      runtime_function_declarations[code] = no;
    }

  return runtime_function_declarations[code];
}

// Make a call to a runtime function.

Call_expression*
Runtime::make_call(Runtime::Function code, Location loc,
		   int param_count, ...)
{
  go_assert(code < Runtime::NUMBER_OF_FUNCTIONS);

  const Runtime_function* pb = &runtime_functions[code];

  go_assert(static_cast<size_t>(param_count)
	     <= sizeof(pb->parameter_types) / sizeof(pb->parameter_types[0]));

  Named_object* no = runtime_declaration(code);
  Expression* func = Expression::make_func_reference(no, NULL, loc);

  Expression_list* args = new Expression_list();
  args->reserve(param_count);

  va_list ap;
  va_start(ap, param_count);
  for (int i = 0; i < param_count; ++i)
    {
      Expression* e = va_arg(ap, Expression*);
      Runtime_function_type rft = pb->parameter_types[i];
      args->push_back(convert_to_runtime_function_type(rft, e, loc));
    }
  va_end(ap);

  return Expression::make_call(func, args, false, loc);
}

// Get the runtime code for a named builtin function.  This is used as a helper
// when creating function references for call expressions.  Every reference to
// a builtin runtime function should have the associated runtime code.  If the
// name is ambiguous and can refer to many runtime codes, return
// NUMBER_OF_FUNCTIONS.

Runtime::Function
Runtime::name_to_code(const std::string& name)
{
  Function code = Runtime::NUMBER_OF_FUNCTIONS;

  // Aliases seen in function declaration code.
  // TODO(cmang): Add other aliases.
  if (name == "new")
    code = Runtime::NEW;
  else if (name == "close")
    code = Runtime::CLOSE;
  else if (name == "copy")
    code = Runtime::COPY;
  else if (name == "append")
    code = Runtime::APPEND;
  else if (name == "delete")
    code = Runtime::MAPDELETE;
  else
    {
      // Look through the known names for a match.
      for (size_t i = 0; i < Runtime::NUMBER_OF_FUNCTIONS; i++)
	{
	  if (strcmp(runtime_functions[i].name, name.c_str()) == 0)
	    code = static_cast<Runtime::Function>(i);
	}
    }
  return code;
}
