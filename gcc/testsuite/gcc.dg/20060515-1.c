/* { dg-do run } */
/* { dg-options "-O1" } */

/* This failed because if conversion didn't handle insv patterns properly.  */

void abort (void);

union y
{
  int a;
  unsigned short b;
};

void __attribute__ ((noinline))
bar (unsigned short u, union y v)
{
  if (u != 1)
    abort ();
}

void __attribute__ ((noinline))
foo (int check)
{
  union y x;

  if (check != 0)
    x.b = 1;
  else
    x.b = 2;
  bar (x.b, x);
}

int
main ()
{
  foo (1);
  return 0;
}
