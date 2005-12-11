/* { dg-options "-fgnu-runtime" } */
/* { dg-do run } */

#include <objc/encoding.h>
#include <stdlib.h>

struct f
{
  _Bool a;
};


int main(void)
{
  if (objc_sizeof_type (@encode (struct f)) != sizeof(struct f))
   abort ();
  return 0;
}
