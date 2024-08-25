/* { dg-options "-march=rv32imc -mabi=ilp32 -fno-late-combine-instructions" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-Os" } } */

/* These loads cannot be compressed because only one compressed reg is
   available (since args are passed in a0-a4, that leaves a5-a7 available, of
   which only a5 is a compressed reg). Therefore the shorten_memrefs pass should
   not attempt to rewrite these loads into a compressible format. It may not
   be possible to avoid this because shorten_memrefs happens before reg alloc.
*/

extern int sub1 (int, int, int, int, int, int, int);

int
load1a (int a0, int a1, int a2, int a3, int a4, int *array)
{
  int a = 0;
  a += array[200];
  a += array[201];
  a += array[202];
  a += array[203];
  return sub1 (a0, a1, a2, a3, a4, 0, a);
}

extern long long sub2 (long long, long long, long long, long long, long long,
           long long, long long);

long long
load2a (long long a0, long long a1, long long a2, long long a3, long long a4,
  long long *array)
{
  int a = 0;
  a += array[200];
  a += array[201];
  a += array[202];
  a += array[203];
  return sub2 (a0, a1, a2, a3, a4, 0, a);
}

/* { dg-final { scan-assembler-not "load1a:\n(\t?\\.\[^\n\]*\n)*\taddi" { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not "load2a:\n.*addi\[ \t\]*\[at\]\[0-9\],\[at\]\[0-9\],\[0-9\]*" { xfail riscv*-*-*  } } } */
