/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-Os -flto" } */
/* { dg-additional-sources pr98599-b.c } */

int b(int x);
int a() { b(5); }
int main() { a(); }
