/* Another test case for over-eager multiple include optimization,
   where the leading "#if !defined" expression is obtained partially,
   or wholly, from macros.  Neil Booth, 30 Sep 2001.  */

/* { dg-do compile } */
/* { dg-options "" } */

extern void abort (void);

/* Each include file should not be subject to MI optimisation, since
   macro definitions can change.  Each header increments the variable
   VAR if it is defined.

   The first set of inclusions gets the headers into CPP's cache, but
   does nothing since VAR is not defined.  The second set should each
   increment VAR, since none of the initial set should have been
   flagged as optimizable.  */

#define EMPTYL
#define EMPTYR
#define NOT !
#define DEFINED defined (guard)
#define NOT_DEFINED ! defined (guard)

#include "mi6a.h"
#include "mi6b.h"
#include "mi6c.h"
#include "mi6d.h"
#include "mi6e.h"

/* Define the macro guard, and redefine the macros to something that
   forces compilation of the conditional blocks.  */
#define guard
#undef EMPTYL
#define EMPTYL 1 ||
#undef EMPTYR
#define EMPTYR || 1
#undef NOT
#define NOT
#undef DEFINED
#define DEFINED 0
#undef NOT_DEFINED
#define NOT_DEFINED 1

#define VAR five

int
main(void)
{
  unsigned int five = 0;

#include "mi6a.h"
#include "mi6b.h"
#include "mi6c.h"
#include "mi6d.h"
#include "mi6e.h"

  if (five != 5)
    abort ();
  return 0;
}
