/* { dg-do compile } */

#import "../objc-obj-c++-shared/TestsuiteObject.h"

@interface obj : TestsuiteObject
{
@public
  int v1;
@package	/* { dg-warning ".@package. presently has the same effect as .@public." } */
  int v2;
@protected
  int v3;
@private
  int v4;
}
- (int) value;
- (void) setValue: (int)number;
@end

@implementation obj : TestsuiteObject

- (int) value { return v1; }
- (void) setValue: (int)number { v1 = number; }

@end

void foo (void)
{
  obj *a;

  [a setValue:2];
  a->v2 = 1;
  a->v3 = [a value] - a->v2;	/* { dg-warning ".v3. is @protected" } */
  a->v4 = a->v3 - 1;		/* { dg-warning ".v4. is @private" } */
  				/* { dg-warning ".v3. is @protected" "" { target *-*-* } 34 } */
}
