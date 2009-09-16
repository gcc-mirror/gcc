/* Basic test of runtime relational comparisons using NaNs and infinities.  */

#include <stdlib.h>
#include "dfp-dbg.h"

#define PASTE2(A,B) A ## B
#define PASTE(A,B) PASTE2(A,B)

/* Override FAILURE from dfp-dbg.h with one that provides additional info.  */
#undef FAILURE
#ifdef DBG
#define FAILURE(OP,KIND) \
  { printf ("failed at line %d: %s for %s values\n", __LINE__, OP, KIND); \
    failures++; }
#else
#define FAILURE(OP,KIND) __builtin_abort ();
#endif

#ifndef WIDTH
#error define WIDTH as decimal float size in bytes
#endif

#if WIDTH == 32
#define DTYPE _Decimal32
#define SUFFIX DF
#define SUFFIX2 d32
#elif WIDTH == 64
#define DTYPE _Decimal64
#define SUFFIX DD
#define SUFFIX2 d64
#elif WIDTH == 128
#define DTYPE _Decimal128
#define SUFFIX DL
#define SUFFIX2 d128
#elif WIDTH == 0
/* This is for testing the test using a type known to work.  */
#define DTYPE double
#define SUFFIX
#define SUFFIX2
#else
#error invalid width for decimal float type
#endif

DTYPE m_two = PASTE(-2.0, SUFFIX);
DTYPE m_one = PASTE(-1.0, SUFFIX);
DTYPE zero  = PASTE(0.0, SUFFIX);
DTYPE one   = PASTE(1.0, SUFFIX);
DTYPE two   = PASTE(2.0, SUFFIX);

volatile DTYPE x, y, z, _nan, inf, m_inf;

void
test_compares (void)
{
  _nan = PASTE(__builtin_nan, SUFFIX2) ("");
  inf =  PASTE(__builtin_inf, SUFFIX2) ();
  m_inf = - PASTE(__builtin_inf, SUFFIX2) ();

  x = PASTE(__builtin_nan, SUFFIX2) ("");
  y = PASTE(__builtin_inf, SUFFIX2) ();
  z = - PASTE(__builtin_inf, SUFFIX2) ();

  /* Less than or equal to with NaN.  */

  if (x <= two)   FAILURE ("<=", "NaN")
  if (x <= zero)  FAILURE ("<=", "NaN")
  if (x <= m_one) FAILURE ("<=", "NaN")
  if (x <= _nan)  FAILURE ("<=", "NaN")
  if (x <= inf)   FAILURE ("<=", "NaN")
  if (x <= m_inf) FAILURE ("<=", "NaN")

  if (two <= x)   FAILURE ("<=", "NaN")
  if (zero <= x)  FAILURE ("<=", "NaN")
  if (m_one <= x) FAILURE ("<=", "NaN")
  if (_nan <= x)  FAILURE ("<=", "NaN")
  if (inf <= x)   FAILURE ("<=", "NaN")
  if (m_inf <= x) FAILURE ("<=", "NaN")

  /* Less than or equal to with infinities, no NaNs.  */

  if (y <= two)      FAILURE ("<=", "inf")
  if (y <= zero)     FAILURE ("<=", "inf")
  if (y <= m_one)    FAILURE ("<=", "inf")
  if (!(two <= y))   FAILURE ("<=", "inf")
  if (!(zero <= y))  FAILURE ("<=", "inf")
  if (!(m_one <= y)) FAILURE ("<=", "inf")

  if (!(z <= two))   FAILURE ("<=", "-inf")
  if (!(z <= zero))  FAILURE ("<=", "-inf")
  if (!(z <= m_one)) FAILURE ("<=", "-inf")
  if (two <= z)      FAILURE ("<=", "-inf")
  if (zero <= z)     FAILURE ("<=", "-inf")
  if (m_one <= z)    FAILURE ("<=", "-inf")

  if (!(y <= inf))   FAILURE ("<=", "inf")
  if (y <= m_inf)    FAILURE ("<=", "inf")
  if (!(z <= inf))   FAILURE ("<=", "inf")
  if (!(z <= m_inf)) FAILURE ("<=", "inf")

  /* Less than with NaN.  */

  if (x < two)       FAILURE ("<", "NaN")
  if (x < zero)      FAILURE ("<", "NaN")
  if (x < m_one)     FAILURE ("<", "NaN")
  if (x < _nan)      FAILURE ("<", "NaN")
  if (x < inf)       FAILURE ("<", "NaN")
  if (x < m_inf)     FAILURE ("<", "NaN")

  if (two < x)       FAILURE ("<", "NaN")
  if (zero < x)      FAILURE ("<", "NaN")
  if (m_one < x)     FAILURE ("<", "NaN")
  if (_nan < x)      FAILURE ("<", "NaN")
  if (inf < x)       FAILURE ("<", "NaN")
  if (m_inf < x)     FAILURE ("<", "NaN")

  /* Less than with infinities, no NaNs.  */

  if (y < two)       FAILURE ("<", "inf")
  if (y < zero)      FAILURE ("<", "inf")
  if (y < m_one)     FAILURE ("<", "inf")
  if (!(two < y))    FAILURE ("<", "inf")
  if (!(zero < y))   FAILURE ("<", "inf")
  if (!(m_one < y))  FAILURE ("<", "inf")

  if (!(z < two))    FAILURE ("<", "-inf")
  if (!(z < zero))   FAILURE ("<", "-inf")
  if (!(z < m_one))  FAILURE ("<", "-inf")
  if (two < z)       FAILURE ("<", "-inf")
  if (zero < z)      FAILURE ("<", "-inf")
  if (m_one < z)     FAILURE ("<", "-inf")

  if (y < inf)       FAILURE ("<=", "inf")
  if (y < m_inf)     FAILURE ("<=", "inf")
  if (!(z < inf))    FAILURE ("<=", "inf")
  if (z < m_inf)     FAILURE ("<=", "inf")

  /* Greater than or equal to with NaN.  */

  if (x >= two)      FAILURE (">=", "NaN")
  if (x >= zero)     FAILURE (">=", "NaN")
  if (x >= m_one)    FAILURE (">=", "NaN")
  if (x >= _nan)     FAILURE (">=", "NaN")
  if (x >= inf)      FAILURE (">=", "NaN")
  if (x >= m_inf)    FAILURE (">=", "NaN")

  if (two >= x)      FAILURE (">=", "NaN")
  if (zero >= x)     FAILURE (">=", "NaN")
  if (m_one >= x)    FAILURE (">=", "NaN")
  if (_nan >= x)     FAILURE (">=", "NaN")
  if (inf >= x)      FAILURE (">=", "NaN")
  if (m_inf >= x)    FAILURE (">=", "NaN")

  /* Greater than or equal to with infinities, no NaNs.  */

  if (!(y >= two))   FAILURE (">=", "inf")
  if (!(y >= zero))  FAILURE (">=", "inf")
  if (!(y >= m_one)) FAILURE (">=", "inf")
  if (two >= y)      FAILURE (">=", "inf")
  if (zero >= y)     FAILURE (">=", "inf")
  if (m_one >= y)    FAILURE (">=", "inf")

  if (z >= two)      FAILURE (">=", "-inf")
  if (z >= zero)     FAILURE (">=", "-inf")
  if (z >= m_one)    FAILURE (">=", "-inf")
  if (!(two >= z))   FAILURE (">=", "-inf")
  if (!(zero >= z))  FAILURE (">=", "-inf")
  if (!(m_one >= z)) FAILURE (">=", "-inf")

  if (!(y >= inf))   FAILURE ("<=", "inf")
  if (!(y >= m_inf)) FAILURE ("<=", "inf")
  if (z >= inf)      FAILURE ("<=", "inf")
  if (!(z >= m_inf)) FAILURE ("<=", "inf")

  /* Greater than with NaN.  */

  if (x > two)       FAILURE (">", "NaN")
  if (x > zero)      FAILURE (">", "NaN")
  if (x > m_one)     FAILURE (">", "NaN")
  if (x > _nan)      FAILURE (">", "NaN")
  if (x > inf)       FAILURE (">", "NaN")
  if (x > m_inf)     FAILURE (">", "NaN")

  if (two > x)       FAILURE (">", "NaN")
  if (zero > x)      FAILURE (">", "NaN")
  if (m_one > x)     FAILURE (">", "NaN")
  if (_nan > x)      FAILURE (">", "NaN")
  if (inf > x)       FAILURE (">", "NaN")
  if (m_inf > x)     FAILURE (">", "NaN")

  /* Greater than with infinities, no NaNs.  */

  if (!(y > two))    FAILURE (">", "inf")
  if (!(y > zero))   FAILURE (">", "inf")
  if (!(y > m_one))  FAILURE (">", "inf")
  if (two > y)       FAILURE (">", "inf")
  if (zero > y)      FAILURE (">", "inf")
  if (m_one > y)     FAILURE (">", "inf")

  if (z > two)       FAILURE (">", "-inf")
  if (z > zero)      FAILURE (">", "-inf")
  if (z > m_one)     FAILURE (">", "-inf")
  if (!(two > z))    FAILURE (">", "-inf")
  if (!(zero > z))   FAILURE (">", "-inf")
  if (!(m_one > z))  FAILURE (">", "-inf")

  if (y > inf)       FAILURE (">", "inf")
  if (!(y > m_inf))  FAILURE (">", "inf")
  if (z > inf)       FAILURE (">", "inf")
  if (z > m_inf)     FAILURE (">", "inf")

  /* Equal with NaN.  */

  if (x == two)      FAILURE ("==", "NaN")
  if (x == zero)     FAILURE ("==", "NaN")
  if (x == m_one)    FAILURE ("==", "NaN")
  if (x == _nan)     FAILURE ("==", "NaN")
  if (x == inf)      FAILURE ("==", "NaN")
  if (x == m_inf)    FAILURE ("==", "NaN")

  if (two == x)      FAILURE ("==", "NaN")
  if (zero == x)     FAILURE ("==", "NaN")
  if (m_one == x)    FAILURE ("==", "NaN")
  if (_nan == x)     FAILURE ("==", "NaN")
  if (inf == x)      FAILURE ("==", "NaN")
  if (m_inf == x)    FAILURE ("==", "NaN")

  /* Equal with infinities, no NaNs.  */

  if (y == two)      FAILURE ("==", "inf")
  if (y == zero)     FAILURE ("==", "inf")
  if (y == m_one)    FAILURE ("==", "inf")
  if (two == y)      FAILURE ("==", "inf")
  if (zero == y)     FAILURE ("==", "inf")
  if (m_one == y)    FAILURE ("==", "inf")

  if (z == two)      FAILURE ("==", "-inf")
  if (z == zero)     FAILURE ("==", "-inf")
  if (z == m_one)    FAILURE ("==", "-inf")
  if (two == z)      FAILURE ("==", "-inf")
  if (zero == z)     FAILURE ("==", "-inf")
  if (m_one == z)    FAILURE ("==", "-inf")

  if (!(y == inf))   FAILURE ("==", "inf")
  if (y == m_inf)    FAILURE ("==", "inf")
  if (z == inf)      FAILURE ("==", "inf")
  if (!(z == m_inf)) FAILURE ("==", "inf")

  /* Not equal with NaN.  */

  if (!(x != two))   FAILURE ("!=", "NaN")
  if (!(x != zero))  FAILURE ("!=", "NaN")
  if (!(x != m_one)) FAILURE ("!=", "NaN")
  if (!(x != _nan))  FAILURE ("!=", "NaN")
  if (!(x != inf))   FAILURE ("!=", "NaN")
  if (!(x != m_inf)) FAILURE ("!=", "NaN")

  if (!(two != x))   FAILURE ("!=", "NaN")
  if (!(zero != x))  FAILURE ("!=", "NaN")
  if (!(m_one != x)) FAILURE ("!=", "NaN")
  if (!(_nan != x))  FAILURE ("!=", "NaN")
  if (!(inf != x))   FAILURE ("!=", "NaN")
  if (!(m_inf != x)) FAILURE ("!=", "NaN")

  /* Not equal with infinities, no NaNs.  */

  if (!(y != two))   FAILURE ("!=", "inf")
  if (!(y != zero))  FAILURE ("!=", "inf")
  if (!(y != m_one)) FAILURE ("!=", "inf")
  if (!(two != y))   FAILURE ("!=", "inf")
  if (!(zero != y))  FAILURE ("!=", "inf")
  if (!(m_one != y)) FAILURE ("!=", "inf")

  if (!(z != two))   FAILURE ("!=", "-inf")
  if (!(z != zero))  FAILURE ("!=", "-inf")
  if (!(z != m_one)) FAILURE ("!=", "-inf")
  if (!(two != z))   FAILURE ("!=", "-inf")
  if (!(zero != z))  FAILURE ("!=", "-inf")
  if (!(m_one != z)) FAILURE ("!=", "-inf")

  if (y != inf)      FAILURE ("!=", "inf")
  if (!(y != m_inf)) FAILURE ("!=", "inf")
  if (!(z != inf))   FAILURE ("!=", "inf")
  if (z != m_inf)    FAILURE ("!=", "inf")
}
