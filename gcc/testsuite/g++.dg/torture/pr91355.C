// PR tree-optimization/91355
// { dg-do run }
// { dg-options "-std=c++14" }

unsigned int d = 0;

struct S {
  S () { d++; }
  S (const S &) { d++; }
  ~S () { d--; }
};

void
foo (int i) throw (int) // { dg-warning "dynamic exception specifications are deprecated" }
{
  if (i == 0)
    throw 3;
  S d;
  throw 3;
}

int
main ()
{
  try { foo (1); } catch (...) {}
  if (d)
    __builtin_abort ();
}
