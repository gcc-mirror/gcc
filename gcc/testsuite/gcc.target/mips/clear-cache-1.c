/* { dg-do compile } */
/* { dg-options "-msynci isa_rev>=2" } */
/* { dg-final { scan-assembler "\tsynci\t" } } */
/* { dg-final { scan-assembler "\tjr.hb\t" } } */
/* { dg-final { scan-assembler-not "_flush_cache|mips_sync_icache|_cacheflush" } } */

NOMIPS16 void f()
{
  int size = 40;
  char *memory = __builtin_alloca(size);
  __builtin___clear_cache(memory, memory + size);
}

