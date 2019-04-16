/* PR tree-optimization/88800 - Spurious -Werror=array-bounds for non-taken
   branch
   Verify that out-of-bounds memcpy calls are not folded when warnings are
   enabled (builtin-memcpy-2.c verifies they're not folded with warnings
   disabled).
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

extern void* memcpy (void*, const void*, __SIZE_TYPE__);

char a1[1], a2[2], a4[4], a8[8], a16[16], a32[32];

void f1 (const void *p)
{
  memcpy (a1, p, sizeof a1 * 2);    /* { dg-warning "\\\[-Warray-bounds" } */
}

void f2 (const void *p)
{
  memcpy (a2, p, sizeof a2 * 2);    /* { dg-warning "\\\[-Warray-bounds" } */
}

void f4 (const void *p)
{
  memcpy (a4, p, sizeof a4 * 2);    /* { dg-warning "\\\[-Warray-bounds" } */
}

void f8 (const void *p)
{
  memcpy (a8, p, sizeof a8 * 2);    /* { dg-warning "\\\[-Warray-bounds" } */
}

void f16 (const void *p)
{
  memcpy (a16, p, sizeof a16 * 2);  /* { dg-warning "\\\[-Warray-bounds" } */
}

void f32 (const void *p)
{
  memcpy (a32, p, sizeof a32 * 2);  /* { dg-warning "\\\[-Warray-bounds" } */
}

/* { dg-final { scan-tree-dump-times "memcpy" 6 "optimized" } } */
