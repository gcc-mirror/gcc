/* Regression test for cpp.  The following input may cause core dumps
   or # line markers in the middle of the line.  */
/* { dg-do preprocess } */

#define foo(string, arg) bar(2, string, arg)

foo ("\
\
\
\
\
\
\
\
\
\
\
",
NULL);

/*
   { dg-final { if ![file exists 990228-1.i] { return }			} }
   { dg-final { set tmp [grep 990228-1.i ".#"]				} }
   { dg-final { if { [string length $tmp] == 0 } \{			} }
   { dg-final {     pass "990228-1.c: linemarkers in middle of line"	} }
   { dg-final { \} else \{						} }
   { dg-final {     fail "990228-1.c: linemarkers in middle of line"    } }
   { dg-final { \}							} }
 */
