// { dg-additional-options "-fmodules-ts" }

export module tdef;
// { dg-module-bmi tdef }

export struct A
{
  typedef int I;
};
