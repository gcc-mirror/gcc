/* { dg-options "-O2 -fno-inline-functions" } */
/* { dg-do run } */

/* Gimplification problem exposed by zsh.  All the side-effects in
   function arguments and in the called expression should happen
   before the actual function call.  */
extern void abort (void);
int A;

typedef void (*fnptr) (void);
fnptr *F;

void
foo (int x)
{
  if (A == x)
    abort ();
}

void
bar (int x, int y)
{
  if (x == 5 || y != 3)
    abort ();
}

void
boz (void)
{
  abort ();
}

void
baz (void)
{
  if (*F != boz)
    abort ();
}

fnptr B[2] = { baz, boz };

main ()
{
  int b, c;

  /* The gimplifier was emitting A++ after the call to foo.  */
  A = 5;
  foo (A++);

  /* The increment to 'b' and 'c' must happen before the call.  However,
     the first argument to bar() must be the original value of 'b', while
     the second argument must be the new value of 'c'.  */
  b = 4;
  c = 2;
  bar (b++, ++c);

  /* This call via function pointer *F should go to baz, but F should
     be incremented before the actual call (i.e., right before the
     call F should be pointing to boz).  */
  F = &B[0];
  (*F++) ();

  return 0;
}
