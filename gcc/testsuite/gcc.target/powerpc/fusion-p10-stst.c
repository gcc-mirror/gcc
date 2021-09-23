/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* Verify store fusion is enabled */

void fuse_stw (int *i, int a, int b, int c)
{
  i[1] = a;
  i[5] = b;
  i[2] = c;
}

void fuse_std (long *i, long a, long b, long c)
{
  i[1] = a;
  i[5] = b;
  i[2] = c;
}

void fuse_stfd (double *i, double a, double b, double c)
{
  i[1] = a;
  i[5] = b;
  i[2] = c;
}

/* { dg-final { scan-assembler-times {stw 4,4\(3\)\n\tstw 6,8\(3\)} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {stw 4,4\(3\)\n\tstw 6,8\(3\)} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {std 4,8\(3\)\n\tstd 6,16\(3\)} 1 { target lp64 } } } */
/* { dg-final { scan-assembler-times {stfd 1,8\(3\)\n\tstfd 3,16\(3\)} 1 } } */

