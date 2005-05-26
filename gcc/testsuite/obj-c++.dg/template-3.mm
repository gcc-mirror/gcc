/* Test for passing arguments to ObjC methods in the context of template
   expansion.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>.  */

/* { dg-do run } */

#include <objc/Object.h>
#include <stdlib.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@interface ObjCClass : Object
{
@public
  int info;
}
-(id) init;
-(id) initWithInformation: (int) whatInfo;
-(id) initWithInformation: (int) whatInfo andInfo: (int) info2;
@end

void foo(int info) {
   ObjCClass *mObj1 = [[ObjCClass alloc] init];
   ObjCClass *mObj2 = [[ObjCClass alloc] initWithInformation: info];
   ObjCClass *mObj3 = [[ObjCClass alloc] initWithInformation: info andInfo: 39];

   CHECK_IF(mObj1->info == 666);
   CHECK_IF(mObj2->info == info);
   CHECK_IF(mObj3->info == info + 39);
}

template <class WrappedObjCClass>
class ObjCObjectWrapper
{
    public:
        ObjCObjectWrapper(int info);
        WrappedObjCClass *mObj1, *mObj2, *mObj3;
};

template <class WrappedObjCClass>
ObjCObjectWrapper<WrappedObjCClass>::ObjCObjectWrapper(int info)
{
    mObj1 = [[WrappedObjCClass alloc] init];
    mObj2 = [[WrappedObjCClass alloc] initWithInformation: info];
    mObj3 = [[WrappedObjCClass alloc] initWithInformation: info andInfo: 67];
}

@implementation ObjCClass
-(id) init {
  return [self initWithInformation:666];
}
-(id) initWithInformation: (int) whatInfo {
  [super init];
  info = whatInfo;
  return self;
}
-(id) initWithInformation: (int) whatInfo andInfo: (int) info2 {
  [super init];
  info = whatInfo + info2;
  return self;
}
@end

ObjCObjectWrapper<ObjCClass> staticInstance(42); 

int main(void) {
  ObjCObjectWrapper<ObjCClass> stackInstance(47);

  foo(89);
  
  CHECK_IF(staticInstance.mObj1->info == 666);
  CHECK_IF(staticInstance.mObj2->info == 42);
  CHECK_IF(staticInstance.mObj3->info == 42 + 67);
  
  CHECK_IF(stackInstance.mObj1->info == 666);
  CHECK_IF(stackInstance.mObj2->info == 47);
  CHECK_IF(stackInstance.mObj3->info == 47 + 67);
  
  return 0;
}
