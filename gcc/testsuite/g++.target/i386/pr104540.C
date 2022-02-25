/* { dg-do compile } */
/* { dg-options "-O2 -fharden-conditional-branches -mforce-drap -mstackrealign --param=max-grow-copy-bb-insns=125" } */

char c;
int i;

void bar(int);

struct S {
  int mi;
  long ml;
  S(int);
};


void foo() {
  int s = c == 0 ? 1 : 2;
  bar(s);
  if (i)
    S s(0);
}
