// { dg-do assemble  }
// { dg-options "-Wreturn-type" }
// Test that we don't get a warning about flowing off the end.

struct A {
  ~A ();
};

int f()
{
  A a1[2];
  A a2[2];
  return 1234567;
}
