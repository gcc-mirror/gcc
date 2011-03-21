/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

/* Test that the compiler is checking the argument of @synchronized(),
   and produce errors when invalid types are used.  */

#include <objc/objc.h>

@interface MyObject
{
  Class isa;
}
@end

@implementation MyObject
@end

@protocol MyProtocol;

typedef MyObject MyObjectTypedef;
typedef MyObject *MyObjectPtrTypedef;
typedef int intTypedef;

typedef struct { float x; float y; } point, *point_ptr;

int test (id object)
{
  int dummy = 0;

  {
    int x;
    @synchronized (x) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }
  }

  {
    intTypedef x;
    @synchronized (x) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }
  }

  {
    int *x;
    @synchronized (x) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }
  }

  {
    point x;
    @synchronized (x) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }
  }

  {
    point_ptr x;
    @synchronized (x) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }
  }

  {
    id x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    id <MyProtocol> x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    MyObject *x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    MyObject <MyProtocol> *x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    static MyObject *x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    MyObjectTypedef *x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    MyObjectTypedef <MyProtocol> *x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    MyObjectPtrTypedef x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  {
    Class x;
    @synchronized (x) /* Ok */
    { dummy++; }
  }

  @synchronized (1) /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }

  @synchronized ("Test") /* { dg-error ".@synchronized. argument is not an object" } */
    { dummy++; }

  @synchronized () /* { dg-error "expected expression" } */
    { dummy++; }

  @synchronized (int) /* { dg-error "expected expression" } */
    { dummy++; }

  return dummy;
}
