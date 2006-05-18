/* { dg-options "-std=c99 -pedantic-errors" } */

void foo1(int (*o)(int p[*])) { }

void foo2(int o[*]);
void foo3(int o[4][*]);

void foo4(int j, int a[j]);
void foo4(int, int a[*]);
void foo4(int, int a[]);
void foo4(int j, int a[j]) {
}

int foo5(int a, int b[*][*], int c[static sizeof(*b)]);
int foo5(int a, int b[10][10], int c[400]) {
  return sizeof (c);
}

int foo6(int a, int b[*][*], int c[static sizeof(*b)]);
int foo6(int a, int b[a][a], int c[sizeof(*b)]) {
  return sizeof (c);
}

void foo7(__typeof__ (int (*)(int o[*])) i);
