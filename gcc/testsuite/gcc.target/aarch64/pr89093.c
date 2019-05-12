/* PR target/89093 */
/* { dg-do compile } */

__attribute__((target ("  no-strict-align"))) void f1 (void) {} /* { dg-error "is not valid" } */
__attribute__((target ("	general-regs-only"))) void f2 (void) {} /* { dg-error "is not valid" } */
#pragma GCC target ("    general-regs-only")	/* { dg-error "is not valid" } */
void f3 (void) {}
