/* { dg-do compile } */

struct svpattern { int x; };

#pragma GCC aarch64 "arm_sve.h" /* { dg-error "'svpattern' referred to as enum" } */
