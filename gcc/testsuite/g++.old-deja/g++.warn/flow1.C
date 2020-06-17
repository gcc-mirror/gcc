// { dg-do assemble  }
// { dg-options "-Wreturn-type" }
// Test that we don't get a warning about flowing off the end.

typedef int int32_t __attribute__((mode (__SI__)));

struct A {
  ~A ();
};

int32_t f()
{
  A a1[2];
  A a2[2];
  return 1234567L;
}
