/* Contributed by Nicola Pero - Thu Mar  8 17:23:59 CET 2001 */
#include "../../objc-obj-c++-shared/TestsuiteObject.m"

@compatibility_alias MyObject TestsuiteObject;

int main (void)
{
  MyObject *object = [MyObject alloc];

  return 0;
}

