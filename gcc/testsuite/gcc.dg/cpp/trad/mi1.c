/* Test "ignore redundant include" facility.

   We must test with C comments, and null directives, outside
   the guard conditional; also, we test guarding with #ifndef and #if
   !defined.  -H is used because cpp might confuse the issue by
   optimizing out #line markers.  This test only passes if each of the
   headers is read exactly once.

   The disgusting regexp in the dg-error line, when stuck into
   dg.exp's compiler-output regexp, matches the correct -H output and
   only the correct -H output.  It has to be all on one line because
   otherwise it will not be interpreted all in one unit.  */

/* { dg-do compile }
   { dg-options "-H -traditional-cpp" }
   { dg-error "mi1c\.h\n\[^\n\]*mi1nd\.h\n\[^\n\]*mi1ndp\.h\n\[^\n\]*mi1x\.h" "redundant include check" { target *-*-* } 0 } */

#include "mi1c.h"
#include "mi1c.h"
#include "mi1c.h"

#include "mi1nd.h"
#include "mi1nd.h"

#include "mi1ndp.h"
#include "mi1ndp.h"

#define MIX_H
#include "mi1x.h"
#include "mi1x.h"

int
main (void)
{
  return a + c + d;
}
