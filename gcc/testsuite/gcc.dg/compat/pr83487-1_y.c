/* { dg-options "-fno-common" { target { hppa*-*-hpux* } } } */
#include "pr83487-1.h"

struct A a;
struct B b;
struct C c;

extern void f1 (int i, int j, int k, int l, int m, int n, int o, struct A);
extern void f2 (int i, int j, int k, int l, int m, int n, int o, struct A, int p, int q);
extern void f3 (int i, int j, int k, int l, int m, int n, int o, struct B, int p, int q);
extern void f4 (int i, int j, int k, int l, int m, int n, int o, struct C, int p, int q);
extern void f5 (int o, struct A);
extern void f6 (int o, struct A, int p, int q);
extern void f7 (int o, struct B, int p, int q);
extern void f8 (int o, struct C, int p, int q);

void
do_test ()
{
  f1 (6, 0, 1, 2, 3, 4, 5, a);
  f2 (6, 0, 1, 2, 3, 4, 5, a, 7, 8);
  f3 (6, 0, 1, 2, 3, 4, 5, b, 7, 8);
  f4 (6, 0, 1, 2, 3, 4, 5, c, 7, 8);
  f5 (5, a);
  f6 (5, a, 7, 8);
  f7 (5, b, 7, 8);
  f8 (5, c, 7, 8);
}
