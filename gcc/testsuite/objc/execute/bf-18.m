#include <objc/objc.h>

@interface MyObject
{
  Class isa;
  int i;
  char c[1];
}
@end

@implementation MyObject
@end

#include "bf-common.h"

