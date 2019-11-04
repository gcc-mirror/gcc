/* { dg-do compile } */

#pragma GCC aarch64 "arm_sve.h" /* { dg-message "note: originally defined here" } */

enum svpattern { FOO }; /* { dg-error {redeclaration of 'enum svpattern'} } */
enum foo { SV_ALL }; /* { dg-error {redeclaration of enumerator 'SV_ALL'} } */
typedef int SV_POW2; /* { dg-error {'SV_POW2' redeclared as different kind of symbol} } */
int SV_VL3; /* { dg-error {'SV_VL3' redeclared as different kind of symbol} } */
