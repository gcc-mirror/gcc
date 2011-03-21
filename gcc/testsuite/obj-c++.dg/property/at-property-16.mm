/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that if you have a property declared in a class and a
   sub-class, the attributes match.  */

@interface MyRootClass
{
  Class isa;
}
@property (assign) id a;                  /* { dg-message "originally specified here" } */
@property (retain) id b;                  /* { dg-message "originally specified here" } */
@property int c;                          /* { dg-message "originally specified here" } */
@property (nonatomic) int d;              /* { dg-message "originally specified here" } */
@property int e;                          /* { dg-message "originally specified here" } */
@property int f;                          /* { dg-message "originally specified here" } */
@property int g;                          /* { dg-message "originally specified here" } */
@property (readonly) int h;               /* Ok */
@property (readonly,getter=getMe) int i;  /* { dg-message "originally specified here" } */
@end

@interface MyClass : MyRootClass
@property (assign) id a;
@property (retain) id b;
@property int c;
@property (nonatomic) int d;
@property int e;
@property int f;
@property int g;
@property (readonly) int h;
@property (readonly,getter=getMe) int i;
@end

@interface MyClass2 : MyRootClass
@property (retain) id a;         /* { dg-warning "assign semantics attributes of property .a. conflict with previous declaration" } */
@property (assign) id b;         /* { dg-warning "assign semantics attributes of property .b. conflict with previous declaration" } */
@property (nonatomic) int c;     /* { dg-warning ".nonatomic. attribute of property .c. conflicts with previous declaration" } */
@property int d;                 /* { dg-warning ".nonatomic. attribute of property .d. conflicts with previous declaration" } */
@property (setter=setX:) int e;  /* { dg-warning ".setter. attribute of property .e. conflicts with previous declaration" } */
@property (getter=x) int f;      /* { dg-warning ".getter. attribute of property .f. conflicts with previous declaration" } */
@property (readonly) int g;      /* { dg-warning ".readonly. attribute of property .g. conflicts with previous declaration" } */
@property (readwrite) int h;     /* Ok */
@property (readonly) int i;      /* { dg-warning ".getter. attribute of property .i. conflicts with previous declaration" } */
@end
