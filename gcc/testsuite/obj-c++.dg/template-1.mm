/* Test for using ObjC classes as C++ template parameters.  */
/* Author:  Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */

#include <objc/Object.h>
#include <stdlib.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@interface Base: Object
- (int) meth;
@end

@interface Derived: Base   
- (int) meth;                
@end

static int count = 0;

template <class T> struct Templ
{
  T *m;
  int i;
  Templ(): i(55), m([[T alloc] init]) { count++; }
  ~Templ() { [m free]; count--; }
};

@implementation Base
- (int) meth { return 333; }
@end

@implementation Derived
- (int) meth { return 666; }
@end
	
int main (void) {
  CHECK_IF(count == 0);
  {
    Templ<Derived> derived;
    CHECK_IF(derived.i == 55 && count == 1);
    Templ<Base> base;
    CHECK_IF(base.i == 55 && count == 2);
    CHECK_IF([base.m meth] == 333);
    CHECK_IF([derived.m meth] == 666);
  }
  CHECK_IF(count == 0);
  return 0;
}
