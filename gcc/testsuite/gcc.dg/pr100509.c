/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-require-alias "" } */

struct X {
  int a;
};
const int a = 0;
static struct X A __attribute__((alias("a")));
void foo() { struct X b = A; }
