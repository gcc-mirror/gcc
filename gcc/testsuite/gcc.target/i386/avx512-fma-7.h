#include <immintrin.h>

#define PASTER2(x,y)		x##y
#define PASTER3(x,y,z)		_mm##x##_##y##_##z
#define TYPE(vec)		PASTER2 (__m, vec)
#define OP(vec, op, suffix)	PASTER3 (vec, op, suffix)
#define DUP(vec, suffix, val)	PASTER3 (vec, set1, suffix) (val)

extern SCALAR bar (void);

type
foo (type x, type y)
{
  SCALAR f = bar ();
  return OP (vec, op, suffix) (x, y, DUP (vec, suffix, f));
}
