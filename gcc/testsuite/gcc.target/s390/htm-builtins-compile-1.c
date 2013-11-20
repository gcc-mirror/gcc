/* This checks the availability of the low-level builtins introduced
   for transactional execution.  */

/* { dg-do compile } */
/* { dg-require-effective-target htm } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

#include <stdint.h>
#include <htmintrin.h>

int global = 0;
uint64_t g;
struct __htm_tdb global_tdb;

int
foo (struct __htm_tdb* tdb, int reg, int *mem, uint64_t *mem64)
{

  int cc;
  int n;

  __builtin_tbegin ((void *)0);
  __builtin_tbegin ((void *)-99999);
  __builtin_tbegin ((void *)99999);
  while (__builtin_tbegin ((void *)0) != 0)
  {
  }
  cc = __builtin_tbegin ((void *)0x12345678);
  cc = __builtin_tbegin (tdb);
  cc = __builtin_tbegin (&global_tdb);
  cc = __builtin_tbegin ((void *)(long long)(reg + 0x12345678));
  cc = __builtin_tbegin ((void *)(long long)(reg));

  __builtin_tbegin_nofloat ((void *)0);
  __builtin_tbegin_nofloat ((void *)-99999);
  __builtin_tbegin_nofloat ((void *)99999);
  cc = __builtin_tbegin_nofloat ((void *)0x12345678);
  cc = __builtin_tbegin_nofloat (tdb);
  cc = __builtin_tbegin_nofloat (&global_tdb);
  cc = __builtin_tbegin_nofloat ((void *)(long long)(reg + 0x12345678));
  cc = __builtin_tbegin_nofloat ((void *)(long long)(reg));

  __builtin_tbegin_retry ((void *)0, 0);
  cc = __builtin_tbegin_retry ((void *)0, 1);
  cc = __builtin_tbegin_retry ((void *)0, -1);
  cc = __builtin_tbegin_retry ((void *)0, 42);
  cc = __builtin_tbegin_retry ((void *)0, reg);
  cc = __builtin_tbegin_retry ((void *)0, *mem);
  cc = __builtin_tbegin_retry ((void *)0, global);
  cc = __builtin_tbegin_retry (tdb, 42);
  cc = __builtin_tbegin_retry (&global_tdb, 42);
  cc = __builtin_tbegin_retry ((void *)0x12345678, global);
  cc = __builtin_tbegin_retry (
	  (void *)(long long) (reg + 0x12345678), global + 1);
  cc = __builtin_tbegin_retry (
	  (void *)(long long)(reg), global - 1);

  __builtin_tbegin_retry_nofloat ((void *)0, 0);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, 1);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, -1);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, 42);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, reg);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, *mem);
  cc = __builtin_tbegin_retry_nofloat ((void *)0, global);
  cc = __builtin_tbegin_retry_nofloat (tdb, 42);
  cc = __builtin_tbegin_retry_nofloat (&global_tdb, 42);
  cc = __builtin_tbegin_retry_nofloat ((void *)0x12345678, global);
  cc = __builtin_tbegin_retry_nofloat (
	  (void *)(long long) (reg + 0x12345678), global + 1);
  cc = __builtin_tbegin_retry_nofloat (
	  (void *)(long long)(reg), global - 1);

  __builtin_tbeginc ();

  __builtin_tx_nesting_depth ();
  n = __builtin_tx_nesting_depth ();

  __builtin_non_tx_store (mem64, 0);
  {
	  const uint64_t val_var = 0x1122334455667788;

	  __builtin_non_tx_store (mem64, val_var);
  }
  __builtin_non_tx_store (mem64, (uint64_t)reg);
  __builtin_non_tx_store (mem64, g);
  __builtin_non_tx_store ((uint64_t *)0, 0);
  __builtin_non_tx_store ((uint64_t *)0x12345678, 0);
  __builtin_non_tx_store (&g, 23);
  __builtin_non_tx_store (&g, reg);
  __builtin_non_tx_store (&g, *mem);
  __builtin_non_tx_store (&g, global);

  __builtin_tend();

  __builtin_tx_assist (0);
  __builtin_tx_assist (1);
  __builtin_tx_assist (reg);
  __builtin_tx_assist (*mem);
  __builtin_tx_assist (global);
}

/* The taborts must go into separate function since they are
   "noreturn".  */

void
tabort1 ()
{
  __builtin_tabort (256);
}

void
tabort2 (int reg)
{
  __builtin_tabort (reg);
}

void
tabort3 (int reg)
{
  /* { dg-final { scan-assembler-times "tabort\t255" 1 } } */
  __builtin_tabort (reg + 255);
}

void
tabort4 (int *mem)
{
  __builtin_tabort (*mem);
}

void
tabort5 ()
{
  __builtin_tabort (global);
}

void
tabort6 (int *mem)
{
  /* Here global + 255 gets reloaded into a reg.  Better would be to
     just reload global or *mem and get the +255 for free as address
     arithmetic.  */
  __builtin_tabort (*mem + 255);
}

void
tabort7 ()
{
  __builtin_tabort (global + 255);
}

void
tabort8 ()
{
  __builtin_tabort (-1);
}


/* Make sure the tdb NULL argument ends up as immediate value in the
   instruction.  */
/* { dg-final { scan-assembler-times "tbegin\t0," 17 } } */
/* { dg-final { scan-assembler-times "tbegin\t" 41 } } */
/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-times "tbeginc\t" 1 } } */
/* { dg-final { scan-assembler-times "tabort\t" 8 } } */
/* { dg-final { scan-assembler "ppa\t" } } */
