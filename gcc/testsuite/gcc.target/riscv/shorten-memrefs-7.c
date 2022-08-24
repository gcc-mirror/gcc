/* { dg-options "-march=rv32imc -mabi=ilp32 -mno-shorten-memrefs" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* Check that these load/stores do not get rewritten into a compressible format
   when shorten_memrefs is disabled.  */

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

/* { dg-final { scan-assembler-not "addi" } } */
