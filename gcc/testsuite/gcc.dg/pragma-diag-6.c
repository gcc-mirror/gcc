/* { dg-do compile } */
#pragma GCC diagnostic error "-Wnoexcept" /* { dg-warning "is valid for C../ObjC.. but not for C" } */
#pragma GCC diagnostic error "-fstrict-aliasing" /* { dg-warning "not an option that controls warnings" } */
#pragma GCC diagnostic error "-Werror" /* { dg-warning "not an option that controls warnings" } */
#pragma GCC diagnostic error "-Wvla2" /* { dg-warning "unknown option after '#pragma GCC diagnostic' kind" } */
/* { dg-message "did you mean '-Wvla'" "" { target *-*-* } .-1 } */
#pragma GCC diagnostic error "-Walla" /* { dg-warning "unknown option after '#pragma GCC diagnostic' kind" } */
/* { dg-message "did you mean '-Wall'" "" { target *-*-* } .-1 } */
#pragma GCC diagnostic warning "-Walla" /* { dg-warning "unknown option after '#pragma GCC diagnostic' kind" } */
/* { dg-message "did you mean '-Wall'" "" { target *-*-* } .-1 } */
int i;
