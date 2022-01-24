/* PR preprocessor/103026 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-Wbidi-chars=unpaired" } */
/* Test raw strings.  */

const char *s1 = R"(a b c LRE‪ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
const char *s2 = R"(a b c RLE‫ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
const char *s3 = R"(a b c LRO‭ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
const char *s4 = R"(a b c FSI⁨ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
const char *s5 = R"(a b c LRI⁦ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
const char *s6 = R"(a b c RLI⁧ 1 2 3)";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
