/* PR lto/114574
 * { dg-do compile }
 * { dg-options "-flto" } */

const struct S * x;
struct S {};
void f(const struct S **);
