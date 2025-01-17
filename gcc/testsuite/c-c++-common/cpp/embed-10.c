/* { dg-do compile } */
/* { dg-options "--embed-directory=${srcdir}/c-c++-common/cpp/embed-dir" } */

const char *p =
#embed <magna-carta.txt> limit (42)
;
/* { dg-error "makes pointer from integer without a cast" "" { target c } .-2 } */
/* { dg-error "expected identifier" "" { target c } .-3 } */
/* { dg-error "invalid conversion" "" { target c++ } .-4 } */
/* { dg-error "expected unqualified-id before (?:numeric constant|'#embed')" "" { target c++ } .-5 } */
