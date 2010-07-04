#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>
#include <objc/objc-api.h>

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

