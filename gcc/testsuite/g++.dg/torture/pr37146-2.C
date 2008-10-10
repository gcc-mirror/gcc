// PR c++/37146
// { dg-do run }

extern "C" void abort ();
int a, b;
struct A { int i:8; int j:8; int k:16; int l:32; } c;

int
f1 (int x)
{
  return x ? a : b;
}

int
f2 (int x)
{
  return x ? c.i : c.j;
}

int
f3 (int x)
{
  return x ? c.i : a;
}

int
f4 (int x)
{
  return x ? c.i : c.k;
}

int
f5 (int x)
{
  return x ? c.l : b;
}

int
main ()
{
  a = 17;
  b = 18;
  c.i = 19;
  c.j = 20;
  c.k = 21;
  c.l = 22;
  if (f1 (1) != a)
    abort ();
  if (f1 (0) != b)
    abort ();
  if (f2 (1) != c.i)
    abort ();
  if (f2 (0) != c.j)
    abort ();
  if (f3 (1) != c.i)
    abort ();
  if (f3 (0) != a)
    abort ();
  if (f4 (1) != c.i)
    abort ();
  if (f4 (0) != c.k)
    abort ();
  if (f5 (1) != c.l)
    abort ();
  if (f5 (0) != b)
    abort ();
}
