/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

#include <objc/encoding.h>
#include <stdlib.h>

union f
{
  char i;
  double f1;
  short t;
};

union g
{
  int i;
};


int main(void)
{
  if (objc_sizeof_type (@encode (union f)) != sizeof(union f))
   abort ();
  if (objc_alignof_type (@encode (union f)) != __alignof__(union f))
   abort ();
  if (objc_sizeof_type (@encode (union g)) != sizeof(union g))
   abort ();
  if (objc_alignof_type (@encode (union g)) != __alignof__(union g))
   abort ();
  return 0;
}
