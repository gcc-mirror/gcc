/* { dg-do compile } */
/* { dg-options "-O2 isa_rev>=2" } */
/* { dg-final { scan-assembler "synci" } } */
/* { dg-final { scan-assembler "jr.hb" } } */
/* { dg-final { scan-assembler-not "_flush_cache" } } */

NOMIPS16 void f()
{
  int size = 40;
  char *memory = __builtin_alloca(size);
  __builtin___clear_cache(memory, memory + size);
}

