/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

struct S { int x; };
void f (int n) { struct { struct S (*p)[n]; } s; }

