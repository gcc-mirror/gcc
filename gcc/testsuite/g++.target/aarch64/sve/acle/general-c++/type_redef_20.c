/* { dg-do compile } */

enum foo { SV_VL4 };
typedef int SV_POW2;
int SV_ALL;

#pragma GCC aarch64 "arm_sve.h" /* { dg-error "'SV_VL4' conflicts with a previous declaration" } */
/* { dg-error "'SV_POW2' redeclared as different kind of entity" "" { target *-*-* } .-1 } */
/* { dg-error "'SV_ALL' redeclared as different kind of entity" "" { target *-*-* } .-2 } */
