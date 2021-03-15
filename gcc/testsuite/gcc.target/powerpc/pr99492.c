/* { dg-do run { target { powerpc*-ibm-aix* } } } */
/* { dg-options "" } */

extern void abort (void);

struct A {
    double _Complex a[64];
};

struct B {
    double b[64];
};

struct C {
    char c1;
    double _Complex c2;
};

struct D {
    char c1;
    double c2;
};

int main() {   
  if (__alignof(double _Complex) != 8)
    abort();

  if (__alignof(struct A) != 8)
    abort();

  if (__alignof(struct C) != 4)
    abort();

  if (__builtin_offsetof(struct C, c2) != 4)
    abort();

  if (__alignof(double) != 8)
    abort();

  if (__alignof(struct B) != 8)
    abort();

  if (__alignof(struct D) != 4)
    abort();

  if (__builtin_offsetof(struct D, c2) != 4)
    abort();

  return 0;
}
