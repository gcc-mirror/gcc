/* { dg-do preprocess } */
/* { dg-options "-pedantic -std=c89" } */

/* This is an extension and therefore gets a warning.  */
#line 5 "cxx-comments-2.c" 3  /* { dg-warning "garbage at end" "#line extension" } */

/* A system header may contain C++ comments irrespective of mode.  */
// C++ comment is not in C89  { dg-bogus "style comment" "bad warning" }

/*
   { dg-final { if ![file exists cxx-comments-2.i] { return }		} }
   { dg-final { set tmp [grep cxx-comments-2.i "is not in C89" line]	} }
   { dg-final { # send_user "$tmp\n"					} }
   { dg-final { if [regexp "is not in C89" $tmp] \{			} }
   { dg-final {     fail "cxx-comments-2: comment strip check"		} }
   { dg-final { \} else \{						} }
   { dg-final {     pass "cxx-comments-2: comment strip check"		} }
   { dg-final { \}							} }
*/

