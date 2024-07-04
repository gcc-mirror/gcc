/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#define STREAMING_COMPATIBLE
#include "test_sme_acle.h"

/*
** undef_za_1:
**	ret
*/
PROTO (undef_za_1, void, ()) { svundef_za (); }

/*
** undef_za_2:
**	ret
*/
PROTO (undef_za_2, void, ())
{
  svzero_za ();
  svundef_za ();
}

/*
** undef_za_3:
**	mov	(w1[2-5]), (?:wzr|#?0)
**	str	za\[\1, 0\], \[x0(?:, #0, mul vl)\]
**	ret
*/
PROTO (undef_za_3, void, (void *ptr))
{
  svzero_za ();
  svundef_za ();
  svstr_za (0, ptr);
}
