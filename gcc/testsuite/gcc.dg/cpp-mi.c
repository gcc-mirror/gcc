/* Test "ignore redundant include" facility.
   We must test with C and C++ comments outside the guard conditional;
   also, we test guarding with #ifndef and #if !defined.
   -H is used because cpp might confuse the issue by optimizing out
   #line markers.  This test only passes if each of the headers is
   read exactly once.

   The disgusting regexp in the dg-error line, when stuck into
   dg.exp's compiler-output regexp, matches the correct -H output and
   only the correct -H output.  It has to be all on one line because
   otherwise it will not be interpreted all in one unit.  */

/* { dg-do compile }
   { dg-options "-H" }
   { dg-error "mic\.h\n\[^\n\]*micc\.h\n\[^\n\]*mind\.h\n\[^\n\]*mindp\.h\n\[^\n\]*mix\.h" "redundant include check" { target *-*-* } 0 } */

#include "cpp-mic.h"
#include "cpp-mic.h"

#include "cpp-micc.h"
#include "cpp-micc.h"

#include "cpp-mind.h"
#include "cpp-mind.h"

#include "cpp-mindp.h"
#include "cpp-mindp.h"

#define CPP_MIX_H
#include "cpp-mix.h"
#include "cpp-mix.h"

int
main (void)
{
  return a + b + c + d;
}
