// PR c++/93046
// { dg-do run }
// { dg-options "" }

int c;

struct S {
  int i;
  S (int i) : i(i) { ++c; }
  S (const S &s): i(s.i) { ++c; }
  operator bool () { return i; }
};

S
foo ()
{
  return S (1) ? : S (2);
}

int main()
{
  S s = foo();
  if (s.i != 1 || c != 2)
    __builtin_abort ();
}
