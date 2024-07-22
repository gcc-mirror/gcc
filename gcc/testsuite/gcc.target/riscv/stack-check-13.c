/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection -fno-asynchronous-unwind-tables -fno-unwind-tables" } */
/* { dg-skip-if "" { *-*-* } { "-g"} } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#define ARG32(X) X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X
#define ARG384(X) ARG32(X),ARG32(X),ARG32(X),ARG32(X),ARG32(X),ARG32(X), ARG32(X),ARG32(X),ARG32(X),ARG32(X),ARG32(X),ARG32(X)
void out1(ARG384(__int128));
int t1(int);

int t3(int x)
{
  if (x < 1000)
    return t1 (x) + 1;

  out1 (ARG384(1));
  return 0;
}



/* This test creates a large (> 1k) outgoing argument area that needs
   to be probed.  We don't test the exact size of the space or the
   exact offset to make the test a little less sensitive to trivial
   output changes.  */
/* { dg-final { scan-assembler-times "sub\\tsp,sp,t0\\n\\tsd\\tzero,1024\\(sp\\)" 1 } } */
