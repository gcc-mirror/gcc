/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** f1:
**	adds	(w[0-9]+), w0, #4
**	csel	w0, \1, wzr, g[te]
**	ret
*/
/*
** f2:
**	adds	(w[0-9]+), w0, #4
**	csel	w0, \1, wzr, g[te]
**	ret
*/
/*
** f3:
**	adds	(w[0-9]+), w0, #5
**	csinc	w0, \1, wzr, gt
**	ret
*/
/*
** f4:
**	adds	(w[0-9]+), w0, #3
**	csinv	w0, \1, wzr, ge
**	ret
*/

#ifndef TYPE
#define TYPE int32_t
#define TYPE_MIN INT32_MIN
#define TYPE_MAX INT32_MAX
#define VALUE -4
#endif

#include <stdint.h>

TYPE __attribute__((noipa))
f1 (TYPE x)
{
  return (x > VALUE ? x - VALUE : 0);
}

TYPE __attribute__((noipa))
f2 (TYPE x)
{
  return (x > VALUE ? x : VALUE) - VALUE;
}

TYPE __attribute__((noipa))
f3 (TYPE x)
{
  return (x > VALUE ? x : VALUE) - (VALUE - 1);
}

TYPE __attribute__((noipa))
f4 (TYPE x)
{
  return (x > VALUE ? x : VALUE) - (VALUE + 1);
}

TYPE __attribute__((noipa))
f5 (TYPE x)
{
  return (x > VALUE ? x : VALUE) - (VALUE + 2);
}

TYPE __attribute__((noipa))
f6 (TYPE x)
{
  return (x > VALUE ? x : VALUE) - (VALUE - 2);
}

int
main (void)
{
  TYPE max_test = TYPE_MAX;
  if (TYPE_MIN < 0 && VALUE < 0)
    max_test += VALUE;

  if (f1 (TYPE_MIN) != 0)
    __builtin_abort ();
  if (f1 (VALUE - 1) != 0)
    __builtin_abort ();
  if (f1 (VALUE) != 0)
    __builtin_abort ();
  if (f1 (VALUE + 1) != 1)
    __builtin_abort ();
  if (f1 (max_test) != max_test - VALUE)
    __builtin_abort ();

  if (f2 (TYPE_MIN) != 0)
    __builtin_abort ();
  if (f2 (VALUE - 1) != 0)
    __builtin_abort ();
  if (f2 (VALUE) != 0)
    __builtin_abort ();
  if (f2 (VALUE + 1) != 1)
    __builtin_abort ();
  if (f2 (max_test) != max_test - VALUE)
    __builtin_abort ();

  if (f3 (TYPE_MIN) != 1)
    __builtin_abort ();
  if (f3 (VALUE - 1) != 1)
    __builtin_abort ();
  if (f3 (VALUE) != 1)
    __builtin_abort ();
  if (f3 (VALUE + 1) != 2)
    __builtin_abort ();
  if (f3 (max_test - 1) != max_test - VALUE)
    __builtin_abort ();

  if (f4 (TYPE_MIN) != -1)
    __builtin_abort ();
  if (f4 (VALUE - 1) != -1)
    __builtin_abort ();
  if (f4 (VALUE) != -1)
    __builtin_abort ();
  if (f4 (VALUE + 1) != 0)
    __builtin_abort ();
  if (f4 (max_test) != max_test - VALUE - 1)
    __builtin_abort ();

  if (f5 (TYPE_MIN) != -2)
    __builtin_abort ();
  if (f5 (VALUE - 1) != -2)
    __builtin_abort ();
  if (f5 (VALUE) != -2)
    __builtin_abort ();
  if (f5 (VALUE + 1) != -1)
    __builtin_abort ();
  if (f5 (max_test) != max_test - VALUE - 2)
    __builtin_abort ();

  if (f6 (TYPE_MIN) != 2)
    __builtin_abort ();
  if (f6 (VALUE - 1) != 2)
    __builtin_abort ();
  if (f6 (VALUE) != 2)
    __builtin_abort ();
  if (f6 (VALUE + 1) != 3)
    __builtin_abort ();
  if (VALUE <= max_test - 2 && f6 (max_test - 2) != max_test - VALUE)
    __builtin_abort ();

  return 0;
}
