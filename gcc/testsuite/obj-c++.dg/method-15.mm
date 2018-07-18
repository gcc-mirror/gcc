/* Check if finding multiple signatures for a method is handled gracefully when method lookup succeeds (see also method-7.m).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-Wstrict-selector-match" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@protocol MyObject
- (id)initWithData:(TestsuiteObject *)data; /* { dg-line MyObject_initWithData } */
@end

@protocol SomeOther
- (id)initWithData:(int)data; /* { dg-line SomeOther_initWithData } */
@end

@protocol MyCoding
- (id)initWithData:(id<MyObject, MyCoding>)data; /* { dg-line MyCoding_initWithData } */
@end

@interface NTGridDataObject: TestsuiteObject <MyCoding>
{
    TestsuiteObject<MyCoding> *_data;
}
+ (NTGridDataObject*)dataObject:(id<MyObject, MyCoding>)data;
@end

@implementation NTGridDataObject
- (id)initWithData:(id<MyObject, MyCoding>)data {
  return data;
}
+ (NTGridDataObject*)dataObject:(id<MyObject, MyCoding>)data
{
    NTGridDataObject *result = [[NTGridDataObject alloc] initWithData:data]; /* { dg-line result_init } */
     /* { dg-warning "multiple methods named .\\-initWithData:. found" "" { target *-*-* } .-1 } */
     /* { dg-message "using .\\-\\(id\\)initWithData:\\(TestsuiteObject \\*\\)data." "" { target *-*-* } MyObject_initWithData } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(id <MyObject, MyCoding>\\)data." "" { target *-*-* } MyCoding_initWithData } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(int\\)data." "" { target *-*-* } SomeOther_initWithData } */

     /* The following warning is a consequence of picking the "wrong" method signature.  */
     /* { dg-warning "passing argument 1 of .initWithData:. from distinct Objective\\-C type" "" { target *-*-* } result_init } */
    return result;
}
@end
