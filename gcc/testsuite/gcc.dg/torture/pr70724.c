/* { dg-do run } */
/* { dg-additional-options "-ftracer" } */

extern void abort (void);

typedef long int _PyTime_t;
typedef enum { _PyTime_ROUND_FLOOR = 0, _PyTime_ROUND_CEILING = 1 }
  _PyTime_round_t;

static _PyTime_t
_PyTime_Divide(const _PyTime_t t, const _PyTime_t k,
	       const _PyTime_round_t round)
{
  if (round == _PyTime_ROUND_CEILING) {
      if (t >= 0)
	return (t + k - 1) / k;
      else
	return t / k;
  }
  else {
      if (t >= 0)
	return t / k;
      else
	return (t - (k - 1)) / k;
  }
}

_PyTime_t __attribute__((noinline,noclone))
_PyTime_AsMicroseconds(_PyTime_t t, _PyTime_round_t round)
{
  return _PyTime_Divide(t, 1000, round);
}

int main()
{
  if (_PyTime_AsMicroseconds (10000, _PyTime_ROUND_FLOOR) != 10)
    abort ();
  return 0;
}
