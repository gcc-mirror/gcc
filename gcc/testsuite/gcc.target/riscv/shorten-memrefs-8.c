/* { dg-options "-march=rv32imc -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-Os" } } */

/* shorten_memrefs should use a correct base address*/

void
store (char *p, int k)
{
  *(int *)(p + 17) = k;
  *(int *)(p + 21) = k;
  *(int *)(p + 25) = k;
  *(int *)(p + 29) = k;
}

int
load (char *p)
{
  int a = 0;
  a += *(int *)(p + 17);
  a += *(int *)(p + 21);
  a += *(int *)(p + 25);
  a += *(int *)(p + 29);
  return a;
}

/* { dg-final { scan-assembler "store:\n(\t?\\.\[^\n\]*\n)*\taddi\ta\[0-7\],a\[0-7\],1" } } */
/* { dg-final { scan-assembler "load:\n(\t?\\.\[^\n\]*\n)*\taddi\ta\[0-7\],a\[0-7\],1" } } */

