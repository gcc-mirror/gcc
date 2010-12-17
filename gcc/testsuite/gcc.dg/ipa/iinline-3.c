/* Verify that call declarations are not redirected according to indirect
   inlining edges too early.  */
/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining"  } */

extern void abort (void);

int bar (int k)
{
  return k+2;
}

int baz (int k)
{
  return k+1;
}

static int foo (int (*p)(int), int i)
{
  return p (i+1);
}

int (*g)(int) = baz;

int main (int argc, char *argv[])
{
  if (foo (bar, 0) != 3)
    abort ();
  if (foo (g, 1) != 3)
    abort ();

  return 0;
}
