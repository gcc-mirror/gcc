// { dg-additional-options "-fmodules-ts" }

export module tdef;
// { dg-module-cmi tdef }

export struct A
{
  typedef int I;
};
