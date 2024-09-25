/* { dg-options "-march=rv64imc -mabi=lp64 -fno-late-combine-instructions" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-Os" } } */

/* shorten_memrefs should rewrite these load/stores into a compressible
   format.  */

void
store1a (int *array, int a)
{
  array[200] = a;
  array[201] = a;
  array[202] = a;
  array[203] = a;
}

void
store2a (long long *array, long long a)
{
  array[200] = a;
  array[201] = a;
  array[202] = a;
  array[203] = a;
}

int
load1r (int *array)
{
  int a = 0;
  a += array[200];
  a += array[201];
  a += array[202];
  a += array[203];
  return a;
}

long long
load2r (long long *array)
{
  int a = 0;
  a += array[200];
  a += array[201];
  a += array[202];
  a += array[203];
  return a;
}

/* { dg-final { scan-assembler "store1a:\n(\t?\\.\[^\n\]*\n)*\taddi" } } */
/* The sd insns in store2a are not rewritten because shorten_memrefs currently
   only optimizes lw and sw.
/* { dg-final { scan-assembler "store2a:\n(\t?\\.\[^\n\]*\n)*\taddi" { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler "load1r:\n(\t?\\.\[^\n\]*\n)*\taddi" } } */
/* The ld insns in load2r are not rewritten because shorten_memrefs currently
   only optimizes lw and sw.
/* { dg-final { scan-assembler "load2r:\n(\t?\\.\[^\n\]*\n)*\taddi" { xfail riscv*-*-* } } } */
