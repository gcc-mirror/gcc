/* PR 21412 */
/* { dg-do compile */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC" } */
/* { dg-require-effective-target tls } */

struct S { int x[10]; };
extern __thread struct S s;
int *foo() { return &s.x[2]; }
