#define UNUSED_MACRO /* { dg-error "UNUSED_MACRO" "" { xfail *-*-* } } */
#include "line-map-3.H" /* { dg-bogus "-:UNUSED_MACRO" "" { xfail *-*-* } } */

/* { dg-do compile } */
/* { dg-additional-options "-Werror=unused-macros" } */

/* PR preprocessor/105608 */
/* This test case is currently xfailed and requires work in libcpp/pch.cc to
   resolve.  Currently, the macro location is incorrectly assigned to line 2
   of the header file when read via PCH, because libcpp doesn't try to
   assign locations relative to the newly loaded line map after restoring
   the PCH.  */

/* In PCH mode we also complain incorrectly about the command line macro -Dwith_PCH
   added by dejagnu; that warning would get suppressed if the macro location were
   correctly restored by libcpp to reflect that it was a command line macro.  */
/* { dg-bogus "-:with_PCH" "" { xfail *-*-* } 2 } */

/* The reason we used -Werror was to prevent pch.exp from rerunning without PCH;
   in that case we would get unnecessary XPASS outputs since the test does work
   fine without PCH.  Once the bug is fixed, remove the -Werror and switch to
   dg-warning.  */
/* { dg-regexp {[^[:space:]]*: some warnings being treated as errors} } */
