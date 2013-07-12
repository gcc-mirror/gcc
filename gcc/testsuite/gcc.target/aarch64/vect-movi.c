/* { dg-do run } */
/* { dg-options "-O3 --save-temps -fno-inline" } */

extern void abort (void);

#define N 16

static void
movi_msl8 (int *__restrict a)
{
  int i;

  /* { dg-final { scan-assembler "movi\\tv\[0-9\]+\.4s, 0xab, msl 8" } } */
  for (i = 0; i < N; i++)
    a[i] = 0xabff;
}

static void
movi_msl16 (int *__restrict a)
{
  int i;

  /* { dg-final { scan-assembler "movi\\tv\[0-9\]+\.4s, 0xab, msl 16" } } */
  for (i = 0; i < N; i++)
    a[i] = 0xabffff;
}

static void
mvni_msl8 (int *__restrict a)
{
  int i;

  /* { dg-final { scan-assembler "mvni\\tv\[0-9\]+\.4s, 0xab, msl 8" } } */
  for (i = 0; i < N; i++)
    a[i] = 0xffff5400;
}

static void
mvni_msl16 (int *__restrict a)
{
  int i;

  /* { dg-final { scan-assembler "mvni\\tv\[0-9\]+\.4s, 0xab, msl 16" } } */
  for (i = 0; i < N; i++)
    a[i] = 0xff540000;
}

int
main (void)
{
  int a[N] = { 0 };
  int i;

#define CHECK_ARRAY(a, val)	\
  for (i = 0; i < N; i++)	\
    if (a[i] != val)		\
      abort ();

  movi_msl8 (a);
  CHECK_ARRAY (a, 0xabff);

  movi_msl16 (a);
  CHECK_ARRAY (a, 0xabffff);

  mvni_msl8 (a);
  CHECK_ARRAY (a, 0xffff5400);

  mvni_msl16 (a);
  CHECK_ARRAY (a, 0xff540000);

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
