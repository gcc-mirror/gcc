/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that when using @synthesize with a readonly property, the
   instance variable can be a specialization of the property type.  */

#include <objc/objc.h>

@protocol MyProtocol
- (void)aMethod;
@end

@interface ClassA
@end

@interface ClassB : ClassA
@end


/* This is all OK.  */
@interface Test
{
  int v;
  float w;
  id x;
  Test *y;
  id <MyProtocol> *z;
  ClassA *a;
  ClassB *b;
  ClassA <MyProtocol> *c;
}
@property (assign, readonly) int v;
@property (assign, readonly) float w;
@property (assign, readonly) id x;
@property (assign, readonly) Test *y;
@property (assign, readonly) id <MyProtocol> *z;
@property (assign, readonly) ClassA *a;
@property (assign, readonly) ClassB *b;
@end

@implementation Test
@synthesize v;
@synthesize w;
@synthesize x;
@synthesize y;
@synthesize z;
@synthesize a;
@synthesize b;
@end


/* This is sometimes OK, sometimes not OK.  */
@interface Test2
{
  int v;                   /* { dg-message "originally specified here" } */
  float w;                 /* { dg-message "originally specified here" } */
  id x;                    /* { dg-message "originally specified here" } */
  Test *y;                 
  id <MyProtocol> *z;      /* { dg-message "originally specified here" } */
  ClassA *a;               /* { dg-message "originally specified here" } */
  ClassB *b;               
}
@property (assign, readonly) float v;
@property (assign, readonly) id w;
@property (assign, readonly) int x;
@property (assign, readonly) id y;
@property (assign, readonly) Test *z;
@property (assign, readonly) ClassB *a;
@property (assign, readonly) ClassA *b;
@end

@implementation Test2
@synthesize v; /* { dg-error "property .v. is using instance variable .v. of incompatible type" } */
@synthesize w; /* { dg-error "property .w. is using instance variable .w. of incompatible type" } */
@synthesize x; /* { dg-error "property .x. is using instance variable .x. of incompatible type" } */
@synthesize y;
@synthesize z; /* { dg-error "property .z. is using instance variable .z. of incompatible type" } */
@synthesize a; /* { dg-error "property .a. is using instance variable .a. of incompatible type" } */
@synthesize b; 
@end
