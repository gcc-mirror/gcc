/* PR tree-optimization/96779 */
/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized -fwrapv" } */

#include <stdbool.h>

bool __attribute__ ((noipa)) f_func(int a)
{
    return -a == a;
}

bool __attribute__ ((noipa)) g_func(unsigned int a)
{
    return -a == a;
}

bool __attribute__ ((noipa)) h_func(short a)
{
    return -a == a;
}

bool __attribute__ ((noipa)) k_func(long a)
{
    return -a == a;
}

int
main (void)
{
  // few randomly generated test cases
  if (f_func (71856034))
    {
      __builtin_abort ();
    }
  if (g_func (71856034))
    {
      __builtin_abort ();
    }
  if (h_func (1744))
    {
      __builtin_abort ();
    }
  if (k_func (68268386))
    {
      __builtin_abort ();
    }
  if (f_func (-112237))
    {
      __builtin_abort ();
    }
  if (g_func (-787116))
    {
      __builtin_abort ();
    }
  if (h_func (-863))
    {
      __builtin_abort ();
    }
  if (k_func (-787116))
    {
      __builtin_abort ();
    }
  if (!f_func (0))
    {
      __builtin_abort ();
    }
  if (!g_func (0))
    {
      __builtin_abort ();
    }
  if (!h_func (0))
    {
      __builtin_abort ();
    }
  if (!k_func (0))
    {
      __builtin_abort ();
    }

  return 0;
}

/* Verify that we have *not* transfered "= -" pattern in any of those functions.  */
/* { dg-final { scan-tree-dump-times "= -" 4 "optimized" } } */
