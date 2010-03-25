/* Test if ObjC++ can distinguish protocol qualifiers from
   template arguments.  */
/* Author:  Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/Object1.h"
#include <stdlib.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@protocol Zone
+ allocFromZone:(void *)zone;
- copyFromZone:(void *)zone; 
@end

@protocol Init <Zone>
+ initialize;
- init; 
@end

@interface Foo: Object
{ @public int val; }
- init;
@end

template <class T, class U> struct X {
  T x; U y;
};

X<int, float> xx;

template <typename T> struct Holder
{
  T *obj;
  static int counter;
  Holder(void) { obj = [[T alloc] init]; }
  ~Holder(void) { [obj free]; --counter; }
  id <Init, Zone> getObjId(void) { return obj; }
  Object <Zone, Init> *getObj(void) { return obj; }
};

typedef Holder <Foo <Init, Zone> > FooHolder;

@implementation Foo
-(id) init {
  [super init];
  val = ++FooHolder::counter;
  return self;
}
@end

template <typename T>
int Holder<T>::counter = 0;

int main (void) {
  CHECK_IF(FooHolder::counter == 0);
  {
    FooHolder holder;
    CHECK_IF(holder.obj->val == 1);
    CHECK_IF(FooHolder::counter == 1);
    FooHolder holder2;
    CHECK_IF(holder2.obj->val == 2);
    CHECK_IF(FooHolder::counter == 2);
  }
  CHECK_IF(FooHolder::counter == 0);
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
