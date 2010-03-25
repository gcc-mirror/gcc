/* Author:  Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"
#include <stdarg.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

#define CHECK_IF(expr) if(!(expr)) abort()

template <class ARR, class TYPE> class TestT
{
public:
  TYPE k;
  int abc(ARR *array) {
    return [array count] * k;
  }
  TestT(TYPE _k): k(_k) { }
};

template <class TYPE>
const char *getDesc(void) {
  return [TYPE name];
}

@class Array;

template <class TYPE>
int abc(TYPE *xyz, Array *array) {
  return [xyz count] + [array count];
}

@interface Array: Object {
  id *arr;
  int count;
}
+ (id)arrayWithObjects:(id)first, ... ;
- (int)count;
@end

@implementation Array
+ (id)arrayWithObjects:(id)first, ... {
  Array *a = [Array new];
  a->count = 0;
  a->arr = (id *) calloc(8, sizeof(id));

  va_list args;
  va_start (args, first);
  
  a->arr[a->count++] = first;

  for (id el; el = va_arg(args, id); a->count++)
    a->arr[a->count] = el;

  return a;
}
- (int)count {
  return count;
}
@end

int main(void) {
  CHECK_IF(!strcmp ([@"Object" cString], getDesc<Object>()));
  CHECK_IF(!strcmp ([@"Array" cString], getDesc<Array>()));

  Array* a1 = [Array arrayWithObjects:@"One", @"Two", @"Three", nil];
  Array* a2 = [Array arrayWithObjects:@"Four", @"Five", nil];

  TestT<Array, int> t(7);
  CHECK_IF(t.abc(a1) + t.abc(a2) == 35);
  CHECK_IF(abc(a1, a2) * t.k == 35);
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
