/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#define N 16

int foo1 ()
{
  int i;
  char ia[N];
  char ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  char ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  /* Not vectorizable, multiplication */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] * ic[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != (char) (ib[i] * ic[i]))
        __builtin_abort ();
    }

  return 0;
}

typedef int half_word;

int foo2 ()
{
  int i;
  half_word ia[N];
  half_word ic[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};
  half_word ib[N] = {0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45};

  /* Not worthwhile, only 2 parts per int */
  for (i = 0; i < N; i++)
    {
      ia[i] = ib[i] + ic[i];
    }

  /* check results:  */
  for (i = 0; i < N; i++)
    {
      if (ia[i] != ib[i] + ic[i])
        __builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-assembler-times {li\s+[a-x0-9]+,0\s+ret} 2 } } */
/* { dg-final { scan-assembler-not {vset} } } */
