/* { dg-do compile } */

_Complex int a;
char b;
void c() {
  if (b != 2 + (long)(a != 0 ^ 0))
    __builtin_abort();
}
