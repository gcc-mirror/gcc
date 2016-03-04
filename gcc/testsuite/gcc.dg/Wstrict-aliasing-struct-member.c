/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct S { int i; long l; };
long x;
struct S foo () { return *(struct S *)&x; } /* { dg-warning "will break strict-aliasing" } */
