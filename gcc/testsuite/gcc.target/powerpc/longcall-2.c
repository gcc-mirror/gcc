/* { dg-do run } */
/* { dg-options "-mlongcall" } */

extern void abort (void);

#define VAL 12345678

int j = VAL;

void
bar (void)
{
  if (j != VAL)
    abort ();
}

int
main (void)
{
  int i = VAL;

  int foo (void)
  {
    if (i != VAL)
      abort ();
  }

  foo ();
  bar ();

  return 0;
}
