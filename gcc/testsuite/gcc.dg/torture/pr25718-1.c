/* { dg-do run } */
/* PR 25718: invalid constant operand to the "bound" insn; error at
   assembly time.  We'll make sure the code is correct to: run a few
   example values.  */

extern void exit (int);
extern void abort (void);
unsigned __attribute__ ((__noinline__)) foo(unsigned a)
{
  unsigned l;
  l = (a >= (~0u - 512) ? (~0u - 512) : a);
  return l;
}

int
main (void)
{
  if (foo ((unsigned) -512) != (unsigned) -513
      || foo ((unsigned) -514) != (unsigned) -514
      || foo ((unsigned) -513) != (unsigned) -513
      || foo ((unsigned) -1) != (unsigned) -513
      || foo (513) != 513
      || foo (0) != 0)
    abort ();

  exit (0);
}
