/* Fixed in r7-2945-g61f46d0e6dd568.
   Simplified from gcc/ipa-devirt.c.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"


typedef struct odr_type_d {
  /* .... */
  int id;
  /* .... */
} *odr_type;
static odr_type **odr_types_ptr;
#define odr_types (*odr_types_ptr) /* { dg-message "pointer 'odr_types_ptr' is dereferenced here" } */

int cond, other_cond;

odr_type some_logic ();

odr_type
get_odr_type (/* ... */)
{
  /* .... */
  odr_type val = NULL;
  /* .... */

  val = some_logic ();

  /* .... */
  if (cond)
    {
      /* .... */
    }
  else if (other_cond)
    {
      odr_types[val->id] = 0; /* { dg-message "in expansion of macro 'odr_types'" } */
      /* .... */
      if (odr_types_ptr) /* { dg-warning "check of 'odr_types_ptr' for NULL after already dereferencing it" } */
	{
	  /* .... */
	  val->id = 42;
	}
      /* .... */
    }
  return val;
}
