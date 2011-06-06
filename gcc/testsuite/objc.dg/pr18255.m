/* This is a test for a GNU Objective-C Runtime library bug.  */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

#include <objc/runtime.h>
#include <objc/Protocol.h>
#include <stdlib.h>

@protocol a
- aMethod;
@end


@protocol b <a>
- bMethod;
@end


int main (int argc, char **argv)
{
  struct objc_method_description m;
  m = protocol_getMethodDescription (@protocol(b), @selector(aMethod), YES, YES);

  if (m.name != NULL)
    abort ();

  m = protocol_getMethodDescription (@protocol(a), @selector(aMethod), YES, YES);

  if (m.name == NULL)
    abort ();

  return 0;
}
