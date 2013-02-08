/* PR tree-optimization/56064 */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -O2" } */

extern void abort (void);
extern void exit (int);

void test_k (void)
{
  _Accum a;
  __INT32_TYPE__ i = -__INT32_MAX__;
  
  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&a, &i, sizeof (a));

  if (a >= 0k)
    abort();
}

void test_0k (void)
{
  _Accum a;
  __INT32_TYPE__ i = 0;
  
  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&a, &i, sizeof (a));

  if (a != 0k)
    abort();
}


void test_hr (void)
{
  short _Fract a;
  __INT8_TYPE__ i = -__INT8_MAX__;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&a, &i, sizeof (a));

  if (a >= 0hr)
    abort();
}

void test_0hr (void)
{
  short _Fract a;
  __INT8_TYPE__ i = 0;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&a, &i, sizeof (a));

  if (a != 0hr)
    abort();
}


void test_si (void)
{
  _Accum a = __ACCUM_MIN__;
  __INT32_TYPE__ i;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&i, &a, sizeof (i));

  if (i >= 0)
    abort();
}

void test_0si (void)
{
  _Accum a = 0;
  __INT32_TYPE__ i;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&i, &a, sizeof (i));

  if (i != 0)
    abort();
}


void test_qi (void)
{
  short _Fract a = __SFRACT_MIN__;
  __INT8_TYPE__ i;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&i, &a, sizeof (i));

  if (i >= 0)
    abort();
}

void test_0qi (void)
{
  short _Fract a = 0hr;
  __INT8_TYPE__ i;

  if (sizeof (a) != sizeof (i))
    return;

  __builtin_memcpy (&i, &a, sizeof (i));

  if (i != 0)
    abort();
}


int main (void)
{
  test_hr();
  test_k();
  test_qi();
  test_si();

  test_0hr();
  test_0k();
  test_0qi();
  test_0si();

  exit (0);

  return 0;
}
