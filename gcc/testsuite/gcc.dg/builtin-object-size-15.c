/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

int
main ()
{
  struct A { char buf1[9]; char buf2[1]; } a;

  if (__builtin_object_size (a.buf1 + (0 + 4), 1) != 5)
    FAIL ();
  char *p = a.buf1;
  p += 1;
  p += 3;
  if (__builtin_object_size (p, 1) != 5)
    FAIL ();
  p = (char *) &a;
  char *q = p + 1;
  char *r = q + 3;
  char *t = r;
  if (r != (char *) &a + 4)
    t = (char *) &a + 1;
  if (__builtin_object_size (t, 1) != 6)
    FAIL ();

  DONE ();
}
