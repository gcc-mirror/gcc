/* PR target/89093 */
/* { dg-do compile } */

__attribute__((target (" arm"))) void f1 (void) {} /* { dg-error "unknown target attribute or pragma ' arm'" } */
__attribute__((target ("   thumb"))) void f2 (void) {} /* { dg-error "unknown target attribute or pragma '   thumb'" } */
__attribute__((target ("arm,  thumb"))) void f3 (void) {} /* { dg-error "unknown target attribute or pragma '  thumb'" } */
__attribute__((target ("thumb,  arm"))) void f4 (void) {} /* { dg-error "unknown target attribute or pragma '  arm'" } */
#pragma GCC target ("    arm")	/* { dg-error "unknown target attribute or pragma '    arm'" } */
void f5 (void) {}
