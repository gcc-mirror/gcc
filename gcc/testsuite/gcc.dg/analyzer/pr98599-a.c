/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-Os -flto" } */
/* { dg-additional-sources pr98599-b.c } */

int b(int x);
int a() { return b(5); }
int main() { return a(); }
