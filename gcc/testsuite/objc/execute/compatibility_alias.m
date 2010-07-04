/* Contributed by Nicola Pero - Thu Mar  8 17:23:59 CET 2001 */
#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>

@compatibility_alias MyObject Object;

int main (void)
{
  MyObject *object = [MyObject alloc];

  return 0;
}
