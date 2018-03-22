/* { dg-do compile } */
/* { dg-options "-Os -march=r2 -mcdx" } */

/* Check generation of R2 CDX br.n, beqz.n, bnez.n instructions.  */

int f (int a, int b, int c)
{
  if (a == 0)
    return b;
  else
    return c;
}

int g (int a, int b, int c)
{
  if (a != 0)
    return b;
  else
    return c;
}

extern int i (int);
extern int j (int);
extern int k (int);

int h (int a, int b)
{
  int x;

  /* As well as the conditional branch for the "if", there has to be
     an unconditional branch from one branch of the "if" to
     the return statement.  We compile this testcase with -Os to
     avoid insertion of a duplicate epilogue in place of the branch.  */
  if (a == b)
    x = i (37);
  else
    x = j (42);
  return x + a + k (x);
}

/* { dg-final { scan-assembler "\tbeqz\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tbnez\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tbeq\t|\tbne\t" } } */
/* { dg-final { scan-assembler "\tbr\\.n\t.*" } } */
