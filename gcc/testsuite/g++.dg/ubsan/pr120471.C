// PR c++/120471
// { dg-do run }
// { dg-options "-fsanitize=undefined" }

volatile int b[1], a[1];

void
foo (int x)
{
  volatile int c = 21;
  volatile int v = (x % 2 ? b : a)[c % 3];
  if (v != 0)
    __builtin_abort ();
}

int
main ()
{
  foo (1);
  foo (2);
}
