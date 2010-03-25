#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>
#include <objc/objc-api.h>

typedef enum 
{
  A, B
} enumeration;

@interface MyObject
{
  enumeration g:4;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

