/* PR lto/114574 */
/* { dg-do compile { target lto } } */
/* { dg-options "-flto" } */

const struct S * x;
struct S {};
void f(const struct S **);
