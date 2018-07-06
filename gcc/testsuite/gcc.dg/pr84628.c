/* PR ipa/84628 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int f0 (void);
__attribute__((error ("err"))) void f1 (void) { f0 (); f0 (); }
__attribute__((error ("err"))) void f2 (void) { f0 (); f0 (); }
/* { dg-bogus "declared with attribute error" "" { target *-*-* } 0 } */
