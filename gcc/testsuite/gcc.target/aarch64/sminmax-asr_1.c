/* { dg-do compile } */
/* { dg-options "-O --save-temps" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+nocssc"

#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))


/*
** minzero:
**	and	w0, w0, w0, asr #31
**	ret
*/

int32_t
minzero (int32_t a)
{
  return MIN (a, 0);
}

/*
** maxzero:
**	bic	w0, w0, w0, asr #31
**	ret
*/

int32_t
maxzero (int32_t a)
{
  return MAX (a, 0);
}

/*
** minzerol:
**	and	x0, x0, x0, asr #63
**	ret
*/

int64_t
minzerol (int64_t a)
{
  return MIN (a, 0);
}

/*
** maxzerol:
**	bic	x0, x0, x0, asr #63
**	ret
*/

int64_t
maxzerol (int64_t a)
{
  return MAX (a, 0);
}

