// Bug: g++ screws up binding levels in a switch statement with cleanups.
// Build don't link:

struct A {
  ~A() { }
};

int f (int i)
{
  switch (i) {
  default:
    A a;
  }
  return 1;
}				// causes compiler segfault
