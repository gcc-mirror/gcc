/* PR 101832: 
   GCC extension accepts the case when a struct with a C99 flexible array
   member is embedded into another struct (possibly recursively).
   __builtin_object_size will treat such struct as flexible size.
   However, when a structure with non-C99 flexible array member, i.e, trailing
   [0], [1], or [4], is embedded into anther struct, the stucture will not
   be treated as flexible size.  */ 
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

#define expect(p, _v) do { \
  size_t v = _v; \
  if (p == v) \
    __builtin_printf ("ok:  %s == %zd\n", #p, p); \
  else {\
    __builtin_printf ("WAT: %s == %zd (expected %zd)\n", #p, p, v); \
    FAIL (); \
  } \
} while (0);


struct A {
  int n;
  char data[];
};

struct B {
  int m;
  struct A a;
};

struct C {
  int q;
  struct B b;
};

struct A0 {
  int n;
  char data[0];
};

struct B0 {
  int m;
  struct A0 a;
};

struct C0 {
  int q;
  struct B0 b;
};

struct A1 {
  int n;
  char data[1];
};

struct B1 {
  int m;
  struct A1 a;
};

struct C1 {
  int q;
  struct B1 b;
};

struct An {
  int n;
  char data[8];
};

struct Bn {
  int m;
  struct An a;
};

struct Cn {
  int q;
  struct Bn b;
};

volatile void *magic1, *magic2;

int main (int argc, char *argv[])
{
  struct B *outer;
  struct C *outest;

  /* Make sure optimization can't find some other object size. */
  outer = (void *)magic1;
  outest = (void *)magic2;

  expect (__builtin_object_size (&outer->a, 1), -1);
  expect (__builtin_object_size (&outest->b, 1), -1);
  expect (__builtin_object_size (&outest->b.a, 1), -1);

  struct B0 *outer0;
  struct C0 *outest0;

  /* Make sure optimization can't find some other object size. */
  outer0 = (void *)magic1;
  outest0 = (void *)magic2;

  expect (__builtin_object_size (&outer0->a, 1), sizeof (outer0->a));
  expect (__builtin_object_size (&outest0->b, 1), sizeof (outest0->b));
  expect (__builtin_object_size (&outest0->b.a, 1), sizeof (outest0->b.a));

  struct B1 *outer1;
  struct C1 *outest1;

  /* Make sure optimization can't find some other object size. */
  outer1 = (void *)magic1;
  outest1 = (void *)magic2;

  expect (__builtin_object_size (&outer1->a, 1), sizeof (outer1->a));
  expect (__builtin_object_size (&outest1->b, 1), sizeof (outest1->b));
  expect (__builtin_object_size (&outest1->b.a, 1), sizeof (outest1->b.a));

  struct Bn *outern;
  struct Cn *outestn;

  /* Make sure optimization can't find some other object size. */
  outern = (void *)magic1;
  outestn = (void *)magic2;

  expect (__builtin_object_size (&outern->a, 1), sizeof (outern->a));
  expect (__builtin_object_size (&outestn->b, 1), sizeof (outestn->b));
  expect (__builtin_object_size (&outestn->b.a, 1), sizeof (outestn->b.a));

  DONE ();
  return 0;
}
