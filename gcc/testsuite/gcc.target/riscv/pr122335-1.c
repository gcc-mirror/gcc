/* { dg-do compile } */
/* { dg-options "-march=rv64gcb_zicond -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" "-Os" "-O3" "-Oz" } } */
int test_05_3 (int x)
{
  return x > 0 ? x + 1 : x;
}

int mirror (int x)
{
  return x <= 0 ? x : x + 1;
}
/* { dg-final { scan-assembler-times "sgt" 2 } } */
/* { dg-final { scan-assembler-not "czero" } } */
