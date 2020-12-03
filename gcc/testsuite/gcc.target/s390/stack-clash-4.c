/* { dg-do compile } */
/* { dg-options "-O1 -m31 -mzarch -fstack-clash-protection" } */

extern void c(char*);

void
a() {
  char *b = __builtin_alloca(3);
  c(b);
}
