/* Check if finding multiple signatures for a method is handled gracefully when method lookup succeeds (see also method-7.m).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-Wstrict-selector-match" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@protocol MyObject
- (id)initWithData:(TestsuiteObject *)data;
@end

@protocol SomeOther
- (id)initWithData:(int)data;
@end

@protocol MyCoding
- (id)initWithData:(id<MyObject, MyCoding>)data;
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
    NTGridDataObject *result = [[NTGridDataObject alloc] initWithData:data];
     /* { dg-warning "multiple methods named .\\-initWithData:. found" "" { target *-*-* } .-1 } */
     /* { dg-message "using .\\-\\(id\\)initWithData:\\(TestsuiteObject \\*\\)data." "" { target *-*-* } 9 } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(id <MyObject, MyCoding>\\)data." "" { target *-*-* } 17 } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(int\\)data." "" { target *-*-* } 13 } */

     /* The following warning is a consequence of picking the "wrong" method signature.  */
     /* { dg-warning "passing argument 1 of .initWithData:. from distinct Objective\\-C type" "" { target *-*-* } 33 } */
    return result;
}
@end
