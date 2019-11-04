/* { dg-do compile } */

enum svpattern { FOO }; /* { dg-message "note: previous definition here" } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-error "multiple definition of 'enum svpattern'" } */
