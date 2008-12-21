/* { dg-do compile } */
/* { dg-options "-O2 -mips32" } */
/* { dg-final { scan-assembler-not "synci" } } */
/* { dg-final { scan-assembler-not "jr.hb" } } */
/* { dg-final { scan-assembler "_flush_cache" } } */

void f()
{
  int size = 40;
  char *memory = __builtin_alloca(size);
  __builtin___clear_cache(memory, memory + size);
}

