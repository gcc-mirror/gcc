/* PR tree-optimization/102232 */
/* { dg-do run } */
/* { dg-options "-O -fdump-tree-optimized" } */

int __attribute__ ((noipa)) foo (int a, int b)
{
  return b * (1 + a / b) - a;
}

int
main (void)
{
  // few randomly generated test cases
  if (foo (71856034, 238) != 212)
    {
      __builtin_abort ();
    }
  if (foo (71856034, 10909) != 1549)
    {
      __builtin_abort ();
    }
  if (foo (20350, 1744) != 578)
    {
      __builtin_abort ();
    }
  if (foo (444813, 88563) != 86565)
    {
      __builtin_abort ();
    }
  if (foo (112237, 63004) != 13771)
    {
      __builtin_abort ();
    }
  if (foo (68268386, 787116) != 210706)
    {
      __builtin_abort ();
    }
  if (foo (-444813, 88563) != 90561)
    {
      __builtin_abort ();
    }
  if (foo (-68268386, 787116) != 1363526)
    {
      __builtin_abort ();
    }

  return 0;
}

/* Verify that multiplication and division has been removed.  */
/* { dg-final { scan-tree-dump-not " \\* " "optimized" } } */
/* { dg-final { scan-tree-dump-not " / " "optimized" } } */