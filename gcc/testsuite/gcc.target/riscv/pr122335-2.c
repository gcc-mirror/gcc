/* { dg-do compile } */
/* { dg-options "-march=rv64gcb_zicond -mabi=lp64d -O2 -g" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-O3" "-Oz" } } */
int f (int x)
{
  int r;
  if (x > 0) { int t = x + 1; r = t; } else r = x;
  return r;
}
/* { dg-final { scan-assembler "sgt" } } */
/* { dg-final { scan-assembler-not "czero" } } */
