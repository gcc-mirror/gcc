/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
- (int) myGetter;
- (int) myGetterB;
- (int) myGetter2;
- (void) mySetter: (int)property;
- (void) mySetterB: (int)property;
- (void) mySetter2: (int)property;

/* Test that all the new property attributes can be parsed.  */
@property (assign)    id property_a;
@property (copy)      id property_b;
@property (atomic)    int property_ca;
@property (nonatomic) int property_c;
@property (readonly)  int property_d;
@property (readwrite) int property_e;
@property (retain)    id property_f;
@property (release)   int property_g;      /* { dg-error "unknown property attribute" } */

@property (getter=myGetter)  int property_h;
@property (setter=mySetter:) int property_i;

/* Now test various problems.  */

@property (readonly, readwrite) int a;    /* { dg-error ".readwrite. attribute conflicts with .readonly. attribute" } */
@property (readonly, setter=mySetterB:) int b; /* { dg-error ".readonly. attribute conflicts with .setter. attribute" } */

@property (assign, retain) id c;          /* { dg-error ".retain. attribute conflicts with .assign. attribute" } */
@property (assign, copy) id d;            /* { dg-error ".copy. attribute conflicts with .assign. attribute" } */
@property (copy, retain) id e;            /* { dg-error ".retain. attribute conflicts with .copy. attribute" } */

@property (atomic, nonatomic) int property_j; /* { dg-error {'nonatomic' attribute conflicts with 'atomic' attribute} } */

@property (setter=mySetter:,setter=mySetter2:)  int f; /* { dg-warning {multiple property 'setter' methods specified, the latest one will be used} } */
@property (getter=myGetter, getter=myGetter2 )  int g; /* { dg-warning {multiple property 'getter' methods specified, the latest one will be used} } */

@end
