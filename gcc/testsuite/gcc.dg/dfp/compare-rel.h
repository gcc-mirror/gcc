/* Basic test of runtime relational comparisons using simple values that
   are not affected by rounding.  */

#include <stdlib.h>
#include "dfp-dbg.h"

#undef FAILURE
#ifdef DBG
#define FAILURE(OP,KIND) \
  { printf ("failed at line %d: %s for %s values\n", __LINE__, OP, KIND); \
    failures++; }
#else
#define FAILURE(OP,KIND) __builtin_abort ();
#endif

#define PASTE2(A,B) A ## B
#define PASTE(A,B) PASTE2(A,B)

#ifndef WIDTH
#error define WIDTH as decimal float size in bytes
#endif

#if WIDTH == 32
#define DTYPE _Decimal32
#define SUFFIX DF
#elif WIDTH == 64
#define DTYPE _Decimal64
#define SUFFIX DD
#elif WIDTH == 128
#define DTYPE _Decimal128
#define SUFFIX DL
#elif WIDTH == 0
/* This is for testing the test using a type known to work.  */
#define DTYPE double
#define SUFFIX
#else
#error invalid width for decimal float type
#endif

DTYPE m_two = PASTE(-2.0, SUFFIX);
DTYPE m_one = PASTE(-1.0, SUFFIX);
DTYPE zero  = PASTE(0.0, SUFFIX);
DTYPE one   = PASTE(1.0, SUFFIX);
DTYPE two   = PASTE(2.0, SUFFIX);

void
test_compares (void)
{
  DTYPE x = one;
  DTYPE y = zero;
  DTYPE z = m_one;

  /* Less than or equal to: comparisons against equal values.  */

  if (! (x <= one))   FAILURE ("<=", "equal")
  if (! (y <= zero))  FAILURE ("<=", "equal")
  if (! (z <= m_one)) FAILURE ("<=", "equal")

  /* Less than or equal to: comparisons against lesser values.  */

  if (x <= m_one)     FAILURE ("<=", "lesser")
  if (x <= zero)      FAILURE ("<=", "lesser")
  if (y <= m_one)     FAILURE ("<=", "lesser")
  if (z <= m_two)     FAILURE ("<=", "lesser")

  /* Less than or equal to: comparisons against greater values.  */

  if (! (x <= two))   FAILURE ("<=", "greater")
  if (! (y <= one))   FAILURE ("<=", "greater")
  if (! (z <= zero))  FAILURE ("<=", "greater")
  if (! (z <= one))   FAILURE ("<=", "greater")

  /* Less than: comparisons against equal values.  */

  if (x < one)        FAILURE ("<", "equal")
  if (y < zero)       FAILURE ("<", "equal")
  if (z < m_one)      FAILURE ("<", "equal")

  /* Less than: comparisons against lesser values.  */

  if (x < m_one)      FAILURE ("<", "lesser")
  if (x < zero)       FAILURE ("<", "lesser")
  if (y < m_one)      FAILURE ("<", "lesser")
  if (z < m_two)      FAILURE ("<", "lesser")

  /* Less than: comparisons against greater values.  */

  if (! (x < two))    FAILURE ("<", "greater")
  if (! (y < one))    FAILURE ("<", "greater")
  if (! (z < zero))   FAILURE ("<", "greater")
  if (! (z < one))    FAILURE ("<", "greater")

  /* Greater than or equal to: comparisons against equal values.  */

  if (! (x >= one))   FAILURE (">=", "equal")
  if (! (y >= zero))  FAILURE (">=", "equal")
  if (! (z >= m_one)) FAILURE (">=", "equal")

  /* Greater than or equal to: comparisons against lesser values.  */

  if (! (x >= m_one)) FAILURE (">=", "lesser")
  if (! (x >= zero))  FAILURE (">=", "lesser")
  if (! (y >= m_one)) FAILURE (">=", "lesser")
  if (! (z >= m_two)) FAILURE (">=", "lesser")

  /* Greater than or equal to: comparisons against greater values.  */

  if (x >= two)       FAILURE (">=", "greater")
  if (y >= one)       FAILURE (">=", "greater")
  if (z >= zero)      FAILURE (">=", "greater")
  if (z >= one)       FAILURE (">=", "greater")

  /* Greater than: comparisons against equal values.  */

  if (x > one)        FAILURE (">", "equal")
  if (y > zero)       FAILURE (">", "equal")
  if (z > m_one)      FAILURE (">", "equal")

  /* Greater than: comparisons against lesser values.  */

  if (! (x > m_one))  FAILURE (">", "lesser")
  if (! (x > zero))   FAILURE (">", "lesser")
  if (! (y > m_one))  FAILURE (">", "lesser")
  if (! (z > m_two))  FAILURE (">", "lesser")

  /* Greater than: comparisons against greater values.  */

  if (x > two)        FAILURE (">", "greater")
  if (y > one)        FAILURE (">", "greater")
  if (z > zero)       FAILURE (">", "greater")
  if (z > one)        FAILURE (">", "greater")
}
