/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test passing varargs of the decimal float types.  */

#include <stdarg.h>

extern void abort (void);
static int failcnt = 0;
                                                                                
/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

static _Decimal32
vararg32 (unsigned arg, ...)
{
  int i;
  va_list ap;
  _Decimal32 result;

  va_start (ap, arg);
  for (i = 0; i <= arg; i++)
    result = va_arg (ap, _Decimal32);
  va_end (ap);
  return result;
}

static _Decimal64
vararg64 (unsigned arg, ...)
{
  int i;
  va_list ap;
  _Decimal64 result;

  va_start (ap, arg);
  for (i = 0; i <= arg; i++)
    result = va_arg (ap, _Decimal64);
  va_end (ap);
  return result;
}

static _Decimal128
vararg128 (unsigned arg, ...)
{
  int i;
  va_list ap;
  _Decimal128 result;

  va_start (ap, arg);
  for (i = 0; i <= arg; i++)
    result = va_arg (ap, _Decimal128);
  va_end (ap);
  return result;
}


int main()
{
  /* _Decimal32 variants.  */
  if (vararg32 (0, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 0.0df)
    FAILURE
  if (vararg32 (1, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 1.0df)
    FAILURE
  if (vararg32 (2, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 2.0df)
    FAILURE
  if (vararg32 (3, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 3.0df)
    FAILURE
  if (vararg32 (4, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 4.0df)
    FAILURE
  if (vararg32 (5, 0.0df, 1.0df, 2.0df, 3.0df, 4.0df, 5.0df) != 5.0df)
    FAILURE

  /* _Decimal64 variants.  */
  if (vararg64 (0, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 0.0dd)
    FAILURE
  if (vararg64 (1, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 1.0dd)
    FAILURE
  if (vararg64 (2, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 2.0dd)
    FAILURE
  if (vararg64 (3, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 3.0dd)
    FAILURE
  if (vararg64 (4, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 4.0dd)
    FAILURE
  if (vararg64 (5, 0.0dd, 1.0dd, 2.0dd, 3.0dd, 4.0dd, 5.0dd) != 5.0dd)
    FAILURE

  /* _Decimal128 variants.  */
  if (vararg128 (0, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 0.0dl)
    FAILURE
  if (vararg128 (1, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 1.0dl)
    FAILURE
  if (vararg128 (2, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 2.0dl)
    FAILURE
  if (vararg128 (3, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 3.0dl)
    FAILURE
  if (vararg128 (4, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 4.0dl)
    FAILURE
  if (vararg128 (5, 0.0dl, 1.0dl, 2.0dl, 3.0dl, 4.0dl, 5.0dl) != 5.0dl)
    FAILURE

  if (failcnt != 0)
    abort ();
  return 0;
}
