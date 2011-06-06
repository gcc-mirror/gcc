/* Fix and continue should not interfere with computation of
   local (static) function addresses.  */
/* Author: Ziemowit Laski <zlaski@apple.com> */
   
/* { dg-do run  { target *-*-darwin* } } */
/* { dg-options "-mfix-and-continue" } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdlib.h>

@class MyTarget, MySet;

int global_value = 0;

@interface MyTargetBuildContext : TestsuiteObject
{
  MyTarget * _target;
  unsigned _cacheInvalDisableCount;
  BOOL _cacheInvalidationNeeded;
  unsigned short _isCreatingDependencies:1;
  unsigned short _isCreatingHeadermap:1;
  unsigned short _haveAddedIdleTimeInvoc:1;
  BOOL _hasSetUpBuildSettings;
}
- (id)initWithTarget:(MyTarget *)target;
- (MyTarget *)target;
@end

@interface MyTargetBuildContext (PrivateMethods)
+ (MySet *)_headerFileExtensions;
@end

@interface MyCountedSet: TestsuiteObject {
@public
  int cardinality;
}
- (id)init;
- (id)sortedArrayUsingFunction:(int (*)(id, id, void *))comparator with:(int)value;
@end

@implementation MyCountedSet
- (id)init {
  cardinality = 5;
  global_value = 17;
  return self;
}
- (id)sortedArrayUsingFunction:(int (*)(id, id, void *))comparator with:(int)value {
  if(value == comparator(self, self, self))
    return self;
  return nil;
}  
@end

@implementation MyTargetBuildContext : TestsuiteObject
- (id)initWithTarget:(MyTarget *)target
{
  self = [super init];
  return self;
}
- (MyTarget *)target
{
  return _target;
}

static int _MyCompareObjectsByDecreasingSetCount (id object1, id object2, MyCountedSet * countedSet)
{
  global_value = 5;
  return countedSet->cardinality;
}
+ (MySet *)_headerFileExtensions
{
  MySet * _headerFileExtensions = 0;
  return _headerFileExtensions;
}
- (void)_recomputeHeadermap
{
  MyCountedSet *set = [MyCountedSet new];
  int (*functionPointer)(id, id, void *) = (int (*)(id, id, void *))_MyCompareObjectsByDecreasingSetCount;
  id result = [set sortedArrayUsingFunction:functionPointer with:5];
}
@end

int main(void) {
  MyTargetBuildContext *ctx = [MyTargetBuildContext new];
  [ctx _recomputeHeadermap];
  if (global_value != 5)
    abort();

  return 0;
}

