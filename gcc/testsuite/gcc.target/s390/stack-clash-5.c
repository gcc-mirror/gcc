/* { dg-do compile } */
/* { dg-options "-O1 -m31 -mzarch -fstack-clash-protection" } */

extern void bar (char*);

void
foo() {
  char a[4000];
  bar (a) ;
}
