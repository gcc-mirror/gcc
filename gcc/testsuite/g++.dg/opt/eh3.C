// PR target/18841
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

int r, i1 = 1, i2 = 2, i3 = 3, i4 = 4, i5 = 5;

struct S
{
  ~S() { r = i1 + i2 + i3 + i4 + i5; }
};

void foo()
{
  S s;
  throw 1;
}

void bar()
{
  try {
    foo();
  } catch (...) {
  }
}

int main()
{
  bar();
  if (r != 1 + 2 + 3 + 4 + 5)
    abort ();
}
