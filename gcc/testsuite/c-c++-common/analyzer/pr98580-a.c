/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-additional-options "-flto" } */
/* { dg-additional-sources pr98580-b.c } */

int a;
int *p = &a;
int foo();
int main() { return foo(); }
