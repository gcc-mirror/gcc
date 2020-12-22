// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

export module the.shop;
// { dg-module-cmi the.shop }

export int for_local_people ()
{
  struct X {int a;};
  X m;
  m.a = 5;
  return m.a;
}
