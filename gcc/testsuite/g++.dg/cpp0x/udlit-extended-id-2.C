// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wbidi-chars=any,ucn" }
bool operator ""_d\u202ae\u202cf (unsigned long long); // { dg-line line1 }
// { dg-error "universal character \\\\u202a is not valid in an identifier" "test1" { target *-*-* } line1 }
// { dg-error "universal character \\\\u202c is not valid in an identifier" "test2" { target *-*-* } line1 }
// { dg-warning "found problematic Unicode character" "test3" { target *-*-* } line1 }
