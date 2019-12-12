/* { dg-do compile } */

struct svpattern { int x; };

#pragma GCC aarch64 "arm_sve.h" /* { dg-error {'svpattern' defined as wrong kind of tag} } */
