// { dg-do run  }
// GROUPS passed initialization
// p2766: Make sure that members are initialized in order of declaration
// in the class, not in order of specification in the mem-initializer list.

extern "C" int printf (const char *, ...);
extern "C" void exit (int);

int count = 0;

void die () { printf ("FAIL\n"); exit (1); }

class bar1 {
public:
  bar1 (int) { if (count != 0) die (); count = 1; }
};

class bar2
{
public:
  bar2 (int) { if (count != 1) die (); count = 2; }
};

class foo
{
public:
  bar1 a;
  bar2 b;
  foo (int, int);
};

// bar1 should get built before bar2
foo::foo (int x, int y) : b(x), a(y) {}

int main()
{
  foo f (1, 2);
  printf ("PASS\n");
}
