/* PR target/95907 */
/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mpower10" } */
/* { dg-warning "switch .-mpower10. is no longer supported" "" {target *-*-*} 0 } */

void f(void) { }
