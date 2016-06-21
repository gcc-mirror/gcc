// PR c++/71528
// { dg-do run }
// { dg-options "-O2" }

extern int &x;
int y;

int &
foo ()
{
  return y;
}

int &x = foo ();

int
main ()
{
  if (&x != &y)
    __builtin_abort ();
}

extern int &x;
