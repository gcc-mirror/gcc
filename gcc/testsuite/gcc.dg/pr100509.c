/* { dg-do compile } */
/* { dg-options "-O" } */

struct X {
  int a;
};
const int a = 0;
static struct X A __attribute__((alias("a")));
void foo() { struct X b = A; }
