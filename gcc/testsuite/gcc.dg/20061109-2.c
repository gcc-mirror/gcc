/* { dg-do compile } */
/* { dg-options "-O1" } */

/* This assignment might be implemented with a mem copy from the
   literal pool with two BLKmode operands.  This produced an ICE on S/390
   since simplify_subreg was called for such a BLKmode operand.  */

struct a
{
  unsigned long b:24;
};

void
foo (struct a *t)
{
  t->b = 32;
}
