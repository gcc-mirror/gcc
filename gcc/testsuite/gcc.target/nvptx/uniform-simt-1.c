/* { dg-options "-O2 -muniform-simt" } */

enum memmodel
{
  MEMMODEL_RELAXED = 0,
};

int a = 0;

int
f (void)
{
  int expected = 1;
  return __atomic_compare_exchange_n (&a, &expected, 0, 0, MEMMODEL_RELAXED,
				      MEMMODEL_RELAXED);
}

/* { dg-final { scan-assembler-times "@%r\[0-9\]*\tatom.global.cas" 1 } } */
