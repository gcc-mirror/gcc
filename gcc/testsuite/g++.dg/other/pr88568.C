// PR c/88568
// { dg-do compile }
// { dg-require-dll "" }

struct S {
  __attribute__((dllimport)) static const char foo[];
};

int
foo (int x)
{
  return S::foo[x];
}
