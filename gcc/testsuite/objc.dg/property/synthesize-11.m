/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, November 2010.  */
/* { dg-do compile } */

/* Test errors when @synthesize is used with bitfield instance variables in an incorrect way.  */

#include <stdlib.h>
#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{
  Class isa;
  int countA : 2;                  /* { dg-message "originally specified here" } */
  int countB : 3;                  /* { dg-message "originally specified here" } */
}
+ (id) initialize;
+ (id) alloc;
- (id) init;
@property int countA;       
@property (nonatomic) short countB;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ (id) alloc { return class_createInstance (self, 0); }
- (id) init { return self; }
@synthesize countA; /* { dg-error ".atomic. property .countA. is using bit-field instance variable .countA." } */
@synthesize countB; /* { dg-error "property .countB. is using instance variable .countB. of incompatible type" } */
@end /* { dg-warning "incomplete implementation of class" } */
/* { dg-warning "method definition for ..setCountA.. not found" "" { target *-*-* } .-1 } */
/* { dg-warning "method definition for ..countA. not found" "" { target *-*-* } .-2 } */
