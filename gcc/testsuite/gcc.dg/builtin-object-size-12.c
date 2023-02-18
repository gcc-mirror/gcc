/* { dg-do run } */
/* { dg-options "-O2" } */

#include "builtin-object-size-common.h"

struct S {
    int len;
    char s[0];
};
int main()
{
  char buf[sizeof (struct S) + 32];
  if (__builtin_object_size (((struct S *)&buf[0])->s, 1) != 32)
    FAIL ();
  if (__builtin_object_size (((struct S *)&buf[1])->s, 1) != 31)
    FAIL ();
  if (__builtin_object_size (((struct S *)&buf[64])->s, 0) != 0)
    FAIL ();

  DONE ();
}
