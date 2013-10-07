/* This checks the availability of the low-level builtins introduced
   for transactional execution.  */

/* { dg-do compile } */
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

  cc = __builtin_tbegin (0);
  cc = __builtin_tbegin (tdb);
  cc = __builtin_tbegin (&global_tdb);

  cc = __builtin_tbegin_nofloat (0);
  cc = __builtin_tbegin_nofloat (&global_tdb);

  cc = __builtin_tbegin_retry (0, 42);
  cc = __builtin_tbegin_retry (0, reg);
  cc = __builtin_tbegin_retry (0, *mem);
  cc = __builtin_tbegin_retry (0, global);
  cc = __builtin_tbegin_retry (tdb, 42);
  cc = __builtin_tbegin_retry (&global_tdb, 42);

  cc = __builtin_tbegin_retry_nofloat (0, 42);
  cc = __builtin_tbegin_retry_nofloat (0, reg);
  cc = __builtin_tbegin_retry_nofloat (0, *mem);
  cc = __builtin_tbegin_retry_nofloat (0, global);
  cc = __builtin_tbegin_retry_nofloat (&global_tdb, 42);

  __builtin_tbeginc ();

  n = __builtin_tx_nesting_depth();

  __builtin_non_tx_store(&g, 23);
  __builtin_non_tx_store(mem64, 23);
  __builtin_non_tx_store(&g, reg);
  __builtin_non_tx_store(&g, *mem);
  __builtin_non_tx_store(&g, global);

  __builtin_tabort (42 + 255);
  __builtin_tabort (reg);
  /* { dg-final { scan-assembler-times "tabort\t255" 1 } } */
  __builtin_tabort (reg + 255);
  __builtin_tabort (*mem);
  __builtin_tabort (global);
  /* Here global + 255 gets reloaded into a reg.  Better would be to
     just reload global or *mem and get the +255 for free as address
     arithmetic.  */
  __builtin_tabort (*mem + 255);
  __builtin_tabort (global + 255);

  __builtin_tend();

  __builtin_tx_assist (23);
  __builtin_tx_assist (reg);
  __builtin_tx_assist (*mem);
  __builtin_tx_assist (global);
}

/* Make sure the tdb NULL argument ends up as immediate value in the
   instruction.  */
/* { dg-final { scan-assembler-times "tbegin\t0," 10 } } */
