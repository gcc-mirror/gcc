/* Test "ignore redundant include" facility.
   We must test with C and C++ comments outside the guard conditional;
   also, we test guarding with #ifndef and #if !defined.  */

/* { dg-do preprocess }
   { dg-options "" } */

#include "cpp-mic.h"
#include "cpp-mic.h"

#include "cpp-micc.h"
#include "cpp-micc.h"

#include "cpp-mind.h"
#include "cpp-mind.h"

#include "cpp-mindp.h"
#include "cpp-mindp.h"

int
main (void)
{
  return a + b + c + d;
}

/*
   { dg-final { if ![file exists cpp-mi.i] { return }		} }

   { dg-final { set tmp [grep cpp-mi.i {cpp-mi.*\.h} line]	} }
   { dg-final { # send_user "$tmp\n" } }
   { dg-final { if [regexp "^{\[0-9\]+ cpp-mic\.h} {\[0-9\]+ cpp-micc\.h} {\[0-9\]+ cpp-mind\.h} {\[0-9\]+ cpp-mindp\.h}$" $tmp] \{ } }
   { dg-final {     pass "cpp-mi.c: redundant include check"	} }
   { dg-final { \} else \{					} }
   { dg-final {     fail "cpp-mi.c: redundant include check"	} }
   { dg-final { \}						} }
*/
