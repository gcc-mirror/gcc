#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>

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

