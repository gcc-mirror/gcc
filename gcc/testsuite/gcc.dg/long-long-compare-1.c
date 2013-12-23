/* Problem noticed on SH for DImode comparison with constants.
/* { dg-do run } */
/* { dg-options "-O0" } */

extern void abort(void);
extern void exit(int);

int test2(long long n)
{
  if (n < 2)
    return 1;
  return 0;
}

int test1(long long n)
{
  if (n < 1)
    return 1;
  return 0;
}

int test0(long long n)
{
  if (n < 0)
    return 1;
  return 0;
}

int test1n(long long n)
{
  if (n < -1LL)
    return 1;
  return 0;
}

int test2n(long long n)
{
  if (n < -2LL)
    return 1;
  return 0;
}

int main()
{
  if (test2n (-1LL))
    abort ();

  if (test2n (-2LL))
    abort ();

  if (test2n (0LL))
    abort ();

  if (test2n (1LL))
    abort ();

  if (test2n (2LL))
    abort ();
 
  if (test1n (-1LL))
    abort ();

  if (!test1n (-2LL))
    abort ();

  if (test1n (0LL))
    abort ();

  if (test1n (1LL))
    abort ();

  if (test1n (2LL))
    abort ();

  if (!test0 (-1LL))
    abort ();

  if (!test0 (-2LL))
    abort ();

  if (test0 (0LL))
    abort ();

  if (test0 (1LL))
    abort ();

  if (test0 (2LL))
    abort ();

  if (!test2 (-1LL))
    abort ();

  if (!test2 (-2LL))
    abort ();

  if (!test2 (0LL))
    abort ();

  if (!test2 (1LL))
    abort ();

  if (test2 (2LL))
    abort ();

  if (!test1 (-1LL))
    abort ();

  if (!test1 (-2LL))
    abort ();

  if (!test1 (0LL))
    abort ();

  if (test1 (1LL))
    abort ();

  if (test1 (2LL))
    abort ();

  exit (0);
}



