/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test that when using @synthesize the instance variable and the
   property have exactly the same type.  */

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
@property (assign) int v;
@property (assign) float w;
@property (assign) id x;
@property (assign) Test *y;
@property (assign) id <MyProtocol> *z;
@property (assign) ClassA *a;
@property (assign) ClassB *b;
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


/* This is not OK.  */
@interface Test2
{
  int v;                   /* { dg-message "originally specified here" } */
  float w;                 /* { dg-message "originally specified here" } */
  id x;                    /* { dg-message "originally specified here" } */
  Test *y;                 /* { dg-message "originally specified here" } */
  id <MyProtocol> *z;      /* { dg-message "originally specified here" } */
  ClassA *a;               /* { dg-message "originally specified here" } */
  ClassB *b;               /* { dg-message "originally specified here" } */
}
@property (assign) float v;
@property (assign) id w;
@property (assign) int x;
@property (assign) id y;
@property (assign) Test *z;
@property (assign) ClassB *a;
@property (assign) ClassA *b;
@end

@implementation Test2
@synthesize v; /* { dg-error "property .v. is using instance variable .v. of incompatible type" } */
@synthesize w; /* { dg-error "property .w. is using instance variable .w. of incompatible type" } */
@synthesize x; /* { dg-error "property .x. is using instance variable .x. of incompatible type" } */
@synthesize y; /* { dg-error "property .y. is using instance variable .y. of incompatible type" } */
@synthesize z; /* { dg-error "property .z. is using instance variable .z. of incompatible type" } */
@synthesize a; /* { dg-error "property .a. is using instance variable .a. of incompatible type" } */
@synthesize b; /* { dg-error "property .b. is using instance variable .b. of incompatible type" } */
@end
