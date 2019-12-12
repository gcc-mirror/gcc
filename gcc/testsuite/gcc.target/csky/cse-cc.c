/* { dg-do compile } */
/* { dg-csky-options "-O2" } */

/* Test that the two comparisons are combined.  This was formerly handled
   by a no-longer-present target-specific pass and is now supposed to
   be handled by generic CSE.  */

int e1, e2;

void func (int a, int b, int c, int d, int f, int g)
{
  e1 = a > b ? f : g;
  e2 = a > b ? c : d;

  return;
}

/* { dg-final { scan-assembler-times "cmp" 1 } } */

