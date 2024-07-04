/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

int sub2(int a, long long b) {
  b = (b << 32) >> 31;
  unsigned int x = a + b;
  return x;
}


/* { dg-final { scan-assembler-times "\tslli\t" 1 } } */
/* { dg-final { scan-assembler-times "\taddw\t" 1 } } */
/* { dg-final { scan-assembler-not "\tsrai\t" } } */
/* { dg-final { scan-assembler-not "\tsh.add\t" } } */

