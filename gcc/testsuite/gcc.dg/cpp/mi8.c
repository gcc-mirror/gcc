/* Test multiple include guards suggestions.  */

/* { dg-do preprocess }
   { dg-options "-H" }
   { dg-message "mi8a\.h\n\[^\n\]*mi8c\.h\n\[^\n\]*mi8b\.h\n\[^\n\]*mi8d\.h\nMultiple include guards may be useful for:\n\[^\n\]*mi8a\.h\n\[^\n\]*mi8d\.h\n" "" { target *-*-* } 0 } */

#include "mi8a.h"
#include "mi8b.h"
