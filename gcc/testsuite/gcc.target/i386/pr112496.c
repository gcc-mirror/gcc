/* { dg-do compile } */
/* { dg-options "-O2" } */

struct T { int x : 24; } v;
void f1(int x) {
  while (v.x - ((v.x <<= 1) - v.x)) ;
}
