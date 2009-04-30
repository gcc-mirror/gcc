/* Check if finding multiple signatures for a method is handled gracefully
   when method lookup succeeds (see also method-7.m).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-options "-Wstrict-selector-match" } */

#include <objc/Object.h>

@protocol MyObject
- (id)initWithData:(Object *)data;
@end

@protocol SomeOther
- (id)initWithData:(int)data;
@end

@protocol MyCoding
- (id)initWithData:(id<MyObject, MyCoding>)data;
@end

@interface NTGridDataObject: Object <MyCoding>
{
    Object<MyCoding> *_data;
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
    /* { dg-warning "multiple methods named .\\-initWithData:. found" "" { target *-*-* } 35 } */
    /* { dg-message "using .\\-\\(id\\)initWithData:\\(Object \\*\\)data." "" { target *-*-* } 11 } */
    /* { dg-message "also found .\\-\\(id\\)initWithData:\\(id <MyObject, MyCoding>\\)data." "" { target *-*-* } 19 } */
    /* { dg-message "also found .\\-\\(id\\)initWithData:\\(int\\)data." "" { target *-*-* } 15 } */

    /* The following warning is a consequence of picking the "wrong" method signature.  */
    /* { dg-warning "passing argument 1 of .initWithData:. from distinct Objective\\-C type" "" { target *-*-* } 35 } */
    return result;
}
@end
