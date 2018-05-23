// { dg-module-do run }

export module the.shop;
// { dg-module-bmi the.shop }

export int for_local_people ()
{
  struct X {int a;};
  X m;
  m.a = 5;
  return m.a;
}
