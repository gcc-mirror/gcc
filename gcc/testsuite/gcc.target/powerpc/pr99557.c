/* { dg-do run { target { powerpc*-ibm-aix* } } } */
/* { dg-options "" } */

void abort (void);

struct A {
  double x[2];
  int y;
};

struct B {
  int i;
  struct A a;
};

struct N {
  double d[2];
};

struct S {
  struct N n;
  float f;
};

struct T {
  char c;
  struct S s;
};

int main() {   
  if (__alignof(struct A) != 8)
    abort();

  if (__alignof(struct B) != 4)
    abort();

  if (__builtin_offsetof(struct B, a) != 4)
    abort();

  if (__alignof(struct N) != 8)
    abort();

  if (__alignof(struct S) != 8)
    abort();

  if (__alignof(struct T) != 4)
    abort();

  if (__builtin_offsetof(struct T, s) != 4)
    abort();

  return 0;
}
