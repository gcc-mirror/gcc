/* { dg-do compile } */
/* { dg-options "-mips32" } */
/* { dg-final { scan-assembler-not "\tsynci" } } */
/* { dg-final { scan-assembler-not "\tjr.hb" } } */
/* { dg-final { scan-assembler "_flush_cache|mips_sync_icache|_cacheflush" } } */

void f()
{
  int size = 40;
  char *memory = __builtin_alloca(size);
  __builtin___clear_cache(memory, memory + size);
}

