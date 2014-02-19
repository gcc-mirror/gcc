/* { dg-do compile } */

void
f4(const foo x) /* { dg-error "10:unknown type name" } */
{}

void
f5(foo x, int i) /* { dg-error "4:unknown type name" } */
{}

void
f6(char c, foo x, ...) /* { dg-error "12:unknown type name" } */
{}
