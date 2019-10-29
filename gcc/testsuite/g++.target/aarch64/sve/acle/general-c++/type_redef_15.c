/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h"

enum svpattern { FOO }; /* { dg-error "multiple definition of 'enum svpattern'" } */
enum foo { SV_ALL }; /* { dg-error "'SV_ALL' conflicts with a previous declaration" } */
typedef int SV_POW2; /* { dg-error "'typedef int SV_POW2' redeclared as different kind of entity" } */
int SV_VL3; /* { dg-error "'int SV_VL3' redeclared as different kind of entity" } */
