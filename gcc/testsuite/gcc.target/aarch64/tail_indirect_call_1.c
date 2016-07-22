/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef void FP (int);

/* { dg-final { scan-assembler-times "br\t" 2 } } */
/* { dg-final { scan-assembler-not "blr\t" } } */
void
f1 (FP fp, int n)
{
  (fp) (n);
}

void
f2 (int n, FP fp)
{
  (fp) (n);
}
