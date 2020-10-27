// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

// from tr1/type_traits
export struct __sfinae_types
{
  // anon struct with tdef name
  typedef struct  { int i; } __two;
};

