/*
 * Contributed by Nicola Pero <nicola@brainstorm.co.uk>
 * Fri Feb  2 11:48:01 GMT 2001
 */

#include "../../objc-obj-c++-shared/Protocol1.h"

@protocol MyProtocol
+ (bycopy id<MyProtocol>) bycopyMethod;
@end

@interface MyObject : Object <MyProtocol> 
@end

@implementation MyObject
+ (bycopy id<MyProtocol>) bycopyMethod
{
  return [MyObject alloc];
}
@end

int main (void)
{
  MyObject *object;

  object = [MyObject bycopyMethod];

   exit (0);
}


