/* Test for handling of function pointer ivars */
/* { dg-do run } */

#include <objc/Object.h>

extern int strcmp(const char *, const char *);
extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

typedef float (*floatfunc)(float, float);

@interface MyObject : Object
{
@public
  int (*ivar)(int, int, int);
  floatfunc ffunc;
}
- init;
@end

int foo(int a, int b, int c) {
  return a + b + c;
}

float bar(float a, float b) {
  return a * b;
}

@implementation MyObject
- init {
  [super init];
  ivar = foo;
  ffunc = bar;
  return self;
}
@end

int main ()
{
  MyObject *obj = [[MyObject alloc] init];
  const char *enc = @encode(MyObject);

  CHECK_IF(obj->ivar(4, 5, 6) == 15);
  CHECK_IF(obj->ffunc(34.0, 45.0) == 34.0 * 45.0);
  CHECK_IF(!strcmp(enc, "{MyObject=#^?^?}"));
  return(0);
}

