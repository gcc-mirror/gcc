/* Test "ignore redundant include" facility, with -C on.

   The disgusting regexp in the dg-error line, when stuck into
   dg.exp's compiler-output regexp, matches the correct -H output and
   only the correct -H output.  It has to be all on one line because
   otherwise it will not be interpreted all in one unit.  */

/* { dg-do preprocess }
   { dg-options "-nostdinc -H -C" }
   { dg-message "mi1c\.h" "redundant include check with -C" { target *-*-* } 0 } */

#include "mi1c.h"
#include "mi1c.h"
