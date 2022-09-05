/* Test C2X support for empty initializers: valid use cases with GNU
   extensions.  */
/* { dg-do run } */
/* { dg-options "-std=gnu2x" } */

extern void exit (int);
extern void abort (void);

void
f (int a)
{
  struct s { volatile int x[a]; };
  struct s b = {};
  for (int i = 0; i < a; i++)
    if (b.x[i] != 0)
      abort ();
  /* Overwrite contents of b.x before second call to make it more likely stack
     contents are nonzero if proper initialization did not occur.  */
  for (int i = 0; i < a; i++)
    b.x[i] = -1;
}

int
main (void)
{
  f (100);
  f (100);
  exit (0);
}
