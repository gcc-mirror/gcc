/* { dg-do preprocess } */
/* { dg-options "-pedantic -std=gnu89" } */

/* You can't do this in your own code... */
// C++ comment is not in C89  { dg-warning "style comment|reported only once" "good warning" }

/* ...but we don't bitch about it more than once.  */
// C++ comment is not in C89  { dg-bogus "style comment" "bad warning" }

/*
   { dg-final { if ![file exists cxx-comments-1.i] { return }		} }
   { dg-final { set tmp [grep cxx-comments-1.i "is not in C89" line]	} }
   { dg-final { # send_user "$tmp\n"					} }
   { dg-final { if [regexp "is not in C89" $tmp] \{			} }
   { dg-final {     fail "cxx-comments-1: comment strip check"		} }
   { dg-final { \} else \{						} }
   { dg-final {     pass "cxx-comments-1: comment strip check"		} }
   { dg-final { \}							} }
*/

