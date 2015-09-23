/* { dg-do compile } */
#pragma GCC diagnostic error "-Wnoexcept" /* { dg-warning "is valid for C../ObjC.. but not for C" } */
#pragma GCC diagnostic error "-fstrict-aliasing" /* { dg-warning "not an option that controls warnings" } */
#pragma GCC diagnostic error "-Werror" /* { dg-warning "not an option that controls warnings" } */
int i;
