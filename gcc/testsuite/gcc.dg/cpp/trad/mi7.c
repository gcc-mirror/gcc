/* Test "ignore redundant include" facility.

   -H is used because cpp might confuse the issue by optimizing out
   #line markers.  This test only passes if the headers is read
   twice.

   The disgusting regexp in the dg-error line, when stuck into
   dg.exp's compiler-output regexp, matches the correct -H output and
   only the correct -H output.  It has to be all on one line because
   otherwise it will not be interpreted all in one unit.  */

/* { dg-do preprocess }
   { dg-options "-H -traditional-cpp" }
   { dg-error "mi7a\.h\n\[^\n\]*mi7a\.h\n\[^\n\]*mi7b\.h\n\[^\n\]*mi7b\.h" "redundant include check" { target *-*-* } 0 } */

#include "mi7a.h"
#include "mi7a.h"
#include "mi7b.h"
#include "mi7b.h"
