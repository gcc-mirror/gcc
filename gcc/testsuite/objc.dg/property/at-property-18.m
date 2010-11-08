/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that if you have a property declared in a class and a
   category, the attributes match.  This is almost the same as
   at-property-16.m, but for a category.  It is a separate file
   because it is difficult to test multiple messages for the same
   line.  */

@interface MyRootClass
{
  Class isa;
}
@property (assign) id a;
@property (retain) id b;
@property int c;
@property (nonatomic) int d;
@property int e;
@property int f;
@property int g;
@property (readonly) int h;
@property (readonly,getter=getMe) int i;
@property (nonatomic) float j;
@end

@interface MyRootClass (Category)
@property (retain) id a;         /* { dg-error "assign semantics attributes of property .a. conflict with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 16 } */
@property (assign) id b;         /* { dg-error "assign semantics attributes of property .b. conflict with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 17 } */
@property (nonatomic) int c;     /* { dg-error ".nonatomic. attribute of property .c. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 18 } */
@property int d;                 /* { dg-error ".nonatomic. attribute of property .d. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 19 } */
@property (setter=setX:) int e;  /* { dg-error ".setter. attribute of property .e. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 20 } */
@property (getter=x) int f;      /* { dg-error ".getter. attribute of property .f. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 21 } */
@property (readonly) int g;      /* { dg-error ".readonly. attribute of property .g. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 22 } */
@property (readwrite) int h;     /* Ok */
@property (readonly) int i;      /* { dg-error ".getter. attribute of property .i. conflicts with previous declaration" } */
                                 /* { dg-message "originally specified here" "" { target *-*-* } 24 } */
@property (nonatomic) float j;   /* Ok */
@end
