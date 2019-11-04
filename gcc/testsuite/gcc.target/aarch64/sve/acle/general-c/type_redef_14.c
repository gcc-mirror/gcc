/* { dg-do compile } */

enum svpattern { FOO }; /* { dg-message "note: originally defined here" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {redeclaration of 'enum svpattern'} } */
