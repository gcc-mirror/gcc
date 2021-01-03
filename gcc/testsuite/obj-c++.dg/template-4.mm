/* Author:  Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-additional-sources "../objc-obj-c++-shared/nsconstantstring-class-impl.mm" } */
// { dg-additional-options "-Wno-objc-root-class" }

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#else
#include "../objc-obj-c++-shared/nsconstantstring-class.h"
#endif

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include "../objc-obj-c++-shared/runtime.h"

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

@interface Array: TestsuiteObject {
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
  CHECK_IF(!strcmp ([@"TestsuiteObject" cString], getDesc<TestsuiteObject>()));
  CHECK_IF(!strcmp ([@"Array" cString], getDesc<Array>()));

  Array* a1 = [Array arrayWithObjects:@"One", @"Two", @"Three", nil];
  Array* a2 = [Array arrayWithObjects:@"Four", @"Five", nil];

  TestT<Array, int> t(7);
  CHECK_IF(t.abc(a1) + t.abc(a2) == 35);
  CHECK_IF(abc(a1, a2) * t.k == 35);
  return 0;
}
