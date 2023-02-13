/* Test BTF extern linkage for functions.

   We expect to see one BTF_KIND_FUNC type with global linkage (foo), and
   one BTF_KIND_FUNC type with extern linkage (extfunc).  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "btt_info: kind=12, kflag=0, linkage=2" 1 } } */
/* { dg-final { scan-assembler-times "btt_info: kind=12, kflag=0, linkage=1" 1 } } */

extern int extfunc(int a, int b);

int foo (int x) {

  int y = extfunc (x, x+1);

  return y;
}
