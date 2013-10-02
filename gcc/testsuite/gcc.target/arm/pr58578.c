
/* PR target/58578 */
/* { dg-do run } */
/* { dg-options "-O1" } */

#include <stdlib.h>

typedef struct {
    long _prec;
    int _flag;
    long _exp;
} __my_st_t;

typedef __my_st_t *__my_st_ptr;

int
_test_fn (__my_st_ptr y, const __my_st_ptr xt)
{
  int inexact;
  if (xt->_exp != -2147483647L)
    {
      (y->_flag = xt->_flag);
    }

  do {
      __my_st_ptr _y = y;
      long _err1 = -2 * xt->_exp;
      long _err2 = 2;
      if (0 < _err1)
	{
	  unsigned long _err = (unsigned long) _err1 + _err2;
	  if (__builtin_expect(!!(_err > _y->_prec + 1), 0))
	    return 2;
	  return 3;
	}
  } while (0);

  return 0;
}

int main ()
{
  __my_st_t x, y;
  long pz;
  int inex;

  x._prec = 914;
  y._exp = 18;
  if (_test_fn (&x, &y))
    {
      abort();
    }
  return 0;
}
