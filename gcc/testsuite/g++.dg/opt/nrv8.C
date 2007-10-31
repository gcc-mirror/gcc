// PR optimization/19531
// forbids NRV on volatile return value.
// { dg-options -O2 }
// { dg-do run }

extern "C" { void abort(); }

struct A
{
  int d;

  A ()                     { d = 123; }
  A (const A & o)          { d = o.d;  }
  A (volatile const A & o) { d = o.d + 2; }
};

A bar()
{
  volatile A l;
  return l;
}

main()
{
  A a = bar ();

  if (a.d != 125)
    abort();

  return 0;
}
