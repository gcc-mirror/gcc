/* PR 21412 */
/* { dg-do compile */
/* { dg-options "-O2 -fPIC" } */

struct S { int x[10]; };
extern __thread struct S s;
int *foo() { return &s.x[2]; }
