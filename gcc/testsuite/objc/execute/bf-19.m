#include <objc/objc.h>

@interface MyObject
{
  Class isa;
  unsigned int i;
  MyObject *object;
}
@end

@implementation MyObject
@end

#include "bf-common.h"
