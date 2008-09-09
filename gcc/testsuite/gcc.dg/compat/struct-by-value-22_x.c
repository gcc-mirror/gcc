#ifndef SKIP_VLA_IN_STRUCT

#ifndef T
#include "compat-common.h"
#include "mixed-struct-defs.h"
#include "mixed-struct-init.h"

#define T(NAME, FIELDS, TYPE, FIELDINIT, FIELDTEST)			\
extern void testva##NAME (int n, ...);					\
									\
void									\
testit##NAME (int n)							\
{									\
  struct S { FIELDS TYPE a[n]; } s;					\
  int i;								\
  FIELDINIT;								\
  for (i = 0; i < n; ++i)						\
    s.a[i] = 12 + n - i;						\
  testva##NAME (n, s, n, s);						\
}

#include "struct-by-value-22_x.c"

#undef T

void
struct_by_value_22_x ()
{
  int n;
DEBUG_INIT

#define T(NAME, FIELDS, TYPE, FIELDINIT, FIELDTEST) testit##NAME (n);

  for (n = 0; n < 16; ++n)
    {
#include "struct-by-value-22_x.c"
      DEBUG_NL;
    }
  for (; n < 110; n += 13)
    {
#include "struct-by-value-22_x.c"
      DEBUG_NL;
    }

DEBUG_FINI

if (fails != 0)
  abort ();
}

#else

#define S(NAME, FIELDS, FIELDINIT, FIELDTEST)				\
  T(c##NAME, FIELDS, char, FIELDINIT, FIELDTEST)			\
  T(s##NAME, FIELDS, short, FIELDINIT, FIELDTEST)			\
  T(u##NAME, FIELDS, unsigned, FIELDINIT, FIELDTEST)			\
  T(d##NAME, FIELDS, double, FIELDINIT, FIELDTEST)
S(E, , do {} while (0), DEBUG_DOT)
S(n, int n;, s.n = n, if (s.n != n) DEBUG_CHECK)
#define U(TYPE)								\
S(TYPE, TYPE s;, init##TYPE (&s.s, n), check##TYPE (s.s, n))
U(Scd)
U(Scdc)
U(Sd)
U(Sdi)
U(Scsds)
U(Scsdsc)
U(Scsdis)
U(Scsdisc)
U(Ssds)
U(Ssdsc)
U(Scssdss)
U(Scssdssc)
U(Sfi)
U(Sfii)
U(Sfifi)
U(Sfiifii)
#undef S
#undef U

#endif

#endif
