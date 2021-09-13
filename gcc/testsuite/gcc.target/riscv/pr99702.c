/* { dg-do compile } */
/* { dg-options "-O" } */
char n;
void *i, *j;
void foo(void) {
  __builtin_memcpy(i, j, n);
}
