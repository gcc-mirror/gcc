// Test that we don't get a warning about flowing off the end.
// Build don't link:
// Special g++ Options: -Wreturn-type

struct A {
  ~A ();
};

int f()
{
  A a1[2];
  A a2[2];
  return 1234567;
}
