/* Test "ignore redundant include" facility.
   This doesn't test for the case where the file is opened, and then ignored
   (the file shouldn't have even been opened).  That would require tracing
   system calls.  It could be done on some systems however.  */

/* We have to test two cases: C comments at the top and C++ comments
   at the top.  */

/*
{ dg-do preprocess }
{ dg-options "-Wp,-lang-c-c++-comments" }
*/

#include "cpp-mic.h"
#include "cpp-mic.h"

#include "cpp-micc.h"
#include "cpp-micc.h"

main ()
{
}

/*
   { dg-final { if ![file exists cpp-mi.i] { return }		} }

   { dg-final { set tmp [grep cpp-mi.i cpp-micc? line]		} }
   { dg-final { # send_user "$tmp\n" } }
   { dg-final { if [regexp "^{\[0-9\]+ cpp-mic} {\[0-9\]+ cpp-micc}$" $tmp] \{ } }
   { dg-final {     pass "cpp-mi.c: redundant include check"	} }
   { dg-final { \} else \{					} }
   { dg-final {     fail "cpp-mi.c: redundant include check"	} }
   { dg-final { \}						} }
*/
