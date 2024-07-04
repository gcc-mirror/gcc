/* PR lto/114574 */
/* { dg-do compile { target lto } } */
/* { dg-options "-flto -std=c23" } */

const struct S * x;
struct S {};
void f(const struct S **);
