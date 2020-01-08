/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 --save-temps" } */

struct a {
  int b;
  char c;
};
struct a d = {1, 16};
struct a *e = &d;

int f = 0;

int main() {
  struct a g = {0, 0 };
  f = 0;

  for (; f <= 1; f++) {
    g = d;
    *e = g;
  }

  if (d.c != 16)
    __builtin_abort();
}
