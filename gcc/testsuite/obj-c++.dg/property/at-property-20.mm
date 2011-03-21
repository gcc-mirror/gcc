/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that if you have a property declared in a class and a
   sub-class, the types match (unless it's a readonly property, in
   which case a "specialization" is enough).  */

@protocol MyProtocolA
- (void) doNothingA;
@end

@protocol MyProtocolB
- (void) doNothingB;
@end

@interface MyRootClass
{
  Class isa;
}
@end

@interface MySubClass1 : MyRootClass
@end

@interface MySubClass2 : MyRootClass
@end

@interface MySubClass3 : MyRootClass <MyProtocolA>
@end

@interface MySubClass4 : MySubClass1
@end

/* Now, the test.  */

@interface MyClass : MyRootClass
{ }
@property (assign) id <MyProtocolA> a;        /* { dg-message "originally specified here" } */
@property int b;                              /* { dg-message "originally specified here" } */
@property float c;                            /* { dg-message "originally specified here" } */
@property (assign) MyRootClass *d;            /* { dg-message "originally specified here" } */
@property (assign) MySubClass1 *e;            /* { dg-message "originally specified here" } */
@property (assign, readonly) MySubClass1 *f;  /* { dg-message "originally specified here" } */
@property (assign) MySubClass3 *g;            /* { dg-message "originally specified here" } */
@property (assign, readonly) MySubClass3 *h;  /* { dg-message "originally specified here"  } */
@end

/* The following are all OK because they are identical.  */
@interface MyClass2 : MyClass
{ }
@property (assign) id a;
@property int b;
@property float c;
@property (assign) MyRootClass *d;
@property (assign) MySubClass1 *e;
@property (assign, readonly) MySubClass1 *f;
@property (assign) MySubClass3 *g;
@property (assign, readonly) MySubClass3 *h;
@end

/* The following are not OK.  */
@interface MyClass3 : MyClass
{ }
@property (assign) MySubClass1 *a;            /* { dg-warning "type of property .a. conflicts with previous declaration" } */
@property float b;                            /* { dg-warning "type of property .b. conflicts with previous declaration" } */
@property int c;                              /* { dg-warning "type of property .c. conflicts with previous declaration" } */
@property (assign) id d;                      /* { dg-warning "type of property .d. conflicts with previous declaration" } */
@property (assign) MyRootClass *e;            /* { dg-warning "type of property .e. conflicts with previous declaration" } */
@property (assign, readonly) MyRootClass *f;  /* { dg-warning "type of property .f. conflicts with previous declaration" } */
@property (assign) MySubClass2 *g;            /* { dg-warning "type of property .g. conflicts with previous declaration" } */
@property (assign, readonly) MySubClass2 *h;  /* { dg-warning "type of property .h. conflicts with previous declaration" } */
@end

/* The following are OK.  */
@interface MyClass4 : MyClass
{ }
@property (assign, readonly) MySubClass4 *f;
@property (assign, readonly) MySubClass3 <MyProtocolB> *h;
@end
