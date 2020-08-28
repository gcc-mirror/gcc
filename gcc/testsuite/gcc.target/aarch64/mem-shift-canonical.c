/* This test is a copy of gcc.dg/torture/pr34330.c: here we are looking for
   specific patterns being matched in the AArch64 backend.  */

/* { dg-do compile } */
/* { dg-options "-Os -ftree-vectorize -dp" } */
/* { dg-require-effective-target lp64 } */


struct T
{
  int t;
  struct { short s1, s2, s3, s4; } *s;
};

void
foo (int *a, int *b, int *c, int *d, struct T *e)
{
  int i;
  for (i = 0; i < e->t; i++)
    {
      e->s[i].s1 = a[i];
      e->s[i].s2 = b[i];
      e->s[i].s3 = c[i];
      e->s[i].s4 = d[i];
    }
}

/* { dg-final { scan-assembler-times "add_lsl_di" 3 } } */
