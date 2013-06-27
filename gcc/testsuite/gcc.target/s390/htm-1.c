/* This checks the availability of the low-level builtins introduced
   for transactional execution.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

#include <stdint.h>
#include <htmintrin.h>

int a = 0;
uint64_t g;

int
foo (struct __htm_tdb* tdb)
{

  int cc;
  int n;

  cc = __builtin_tbegin (0);
  cc = __builtin_tbegin (tdb);
  cc = __builtin_tbegin_nofloat (0);
  cc = __builtin_tbegin_nofloat (tdb);
  cc = __builtin_tbegin_retry (0, 42);
  cc = __builtin_tbegin_retry (tdb, 42);
  cc = __builtin_tbegin_retry_nofloat (0, 42);
  cc = __builtin_tbegin_retry_nofloat (tdb, 42);
  __builtin_tbeginc ();
  n = __builtin_tx_nesting_depth();
  __builtin_non_tx_store(&g, n);
  __builtin_tabort (42 + 255);
  __builtin_tend();
  __builtin_tx_assist (23);
}
/* Make sure the tdb NULL argument ends up as immediate value in the
   instruction.  */
/* { dg-final { scan-assembler-times "tbegin\t0," 4 } } */
