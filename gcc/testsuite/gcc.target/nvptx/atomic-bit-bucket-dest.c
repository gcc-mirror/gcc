/* { dg-do compile } */
/* { dg-options "-O2 -misa=sm_35" } */

enum memmodel
{
  MEMMODEL_RELAXED = 0
};

unsigned long long int *p64;
unsigned long long int v64;

int
main()
{
  __atomic_fetch_add (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_and (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_or (p64, v64, MEMMODEL_RELAXED);
  __atomic_fetch_xor (p64, v64, MEMMODEL_RELAXED);
  __atomic_exchange_n (p64, v64, MEMMODEL_RELAXED);

  {
    unsigned long long expected = v64;
    __atomic_compare_exchange_n (p64, &expected, 0, 0, MEMMODEL_RELAXED,
				 MEMMODEL_RELAXED);
  }

  return 0;
}

/* { dg-final { scan-assembler-times "atom.add.u64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "atom.and.b64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "atom.or.b64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "atom.xor.b64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "atom.exch.b64\[\t \]+_," 1 } } */
/* { dg-final { scan-assembler-times "atom.cas.b64\[\t \]+_," 1 } } */
