/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct A
{
  int i, j, k;
  char buf[255];
  int l, m, n, o;
};

int
main (void)
{
  const size_t s = sizeof (struct A);
  const size_t o = __builtin_offsetof (struct A, buf);
  struct A *a = malloc (s);
  struct A *b = malloc (o + 212);
  if (__builtin_object_size (a->buf, 0) != s - o)
    FAIL ();
  if (__builtin_object_size (a->buf, 1) != sizeof (a->buf))
    FAIL ();
  if (__builtin_object_size (a->buf, 2) != s - o)
    FAIL ();
  if (__builtin_object_size (a->buf, 3) != sizeof (a->buf))
    FAIL ();
  if (__builtin_object_size (&a->buf[0], 0) != s - o)
    FAIL ();
  if (__builtin_object_size (&a->buf[0], 1) != sizeof (a->buf))
    FAIL ();
  if (__builtin_object_size (&a->buf[0], 2) != s - o)
    FAIL ();
  if (__builtin_object_size (&a->buf[0], 3) != sizeof (a->buf))
    FAIL ();
  if (__builtin_object_size (&a->buf[6], 0) != s - o - 6)
    FAIL ();
  if (__builtin_object_size (&a->buf[6], 1) != sizeof (a->buf) - 6)
    FAIL ();
  if (__builtin_object_size (&a->buf[6], 2) != s - o - 6)
    FAIL ();
  if (__builtin_object_size (&a->buf[6], 3) != sizeof (a->buf) - 6)
    FAIL ();
  if (__builtin_object_size (b->buf, 0) != 212)
    FAIL ();
  if (__builtin_object_size (b->buf, 1) != 212)
    FAIL ();
  if (__builtin_object_size (b->buf, 2) != 212)
    FAIL ();
  if (__builtin_object_size (b->buf, 3) != 212)
    FAIL ();
  if (__builtin_object_size (&b->buf[0], 0) != 212)
    FAIL ();
  if (__builtin_object_size (&b->buf[0], 1) != 212)
    FAIL ();
  if (__builtin_object_size (&b->buf[0], 2) != 212)
    FAIL ();
  if (__builtin_object_size (&b->buf[0], 3) != 212)
    FAIL ();
  if (__builtin_object_size (&b->buf[28], 0) != 212 - 28)
    FAIL ();
  if (__builtin_object_size (&b->buf[28], 1) != 212 - 28)
    FAIL ();
  if (__builtin_object_size (&b->buf[28], 2) != 212 - 28)
    FAIL ();
  if (__builtin_object_size (&b->buf[28], 3) != 212 - 28)
    FAIL ();
  DONE ();
}
