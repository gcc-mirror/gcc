/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

#define N 16

unsigned long long vals[N];
unsigned int res[N];
unsigned int expects[N] = {0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

unsigned long long inputs[N]
  = {0x0000000000000000ULL, 0x0000000000000001ULL, 0x8000000000000000ULL,
     0x0000000000000002ULL, 0x4000000000000000ULL, 0x0000000100000000ULL,
     0x0000000080000000ULL, 0xa5a5a5a5a5a5a5a5ULL, 0x5a5a5a5a5a5a5a5aULL,
     0xcafecafe00000000ULL, 0x0000cafecafe0000ULL, 0x00000000cafecafeULL,
     0x8070600000000000ULL, 0xffffffffffffffffULL};

__attribute__ ((noipa)) void
init ()
{
  for (int i = 0; i < N; i++)
    vals[i] = inputs[i];
}

__attribute__ ((noipa)) void
do_parity ()
{
  for (int i = 0; i < N; i++)
    res[i] = __builtin_parityll (vals[i]);
}

int
main (void)
{
  init ();
  do_parity ();
  for (int i = 0; i < N; i++)
    if (res[i] != expects[i])
      __builtin_abort();

  return 0;
}

