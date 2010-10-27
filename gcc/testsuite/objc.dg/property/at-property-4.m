/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
- (id) myGetter;
- (id) myGetterB;
- (void) mySetter: (id)property;
- (void) mySetterB: (id)property;

/* Test that all the new property attributes can be parsed.  */
@property (assign)    id property_a;
@property (copy)      id property_b;
@property (nonatomic) id property_c;
@property (readonly)  id property_d;
@property (readwrite) id property_e;
@property (retain)    id property_f;
@property (release)   id property_g;      /* { dg-error "unknown property attribute" } */

/* The following will be enabled when @synthesized is implemented.  */
/* @property (getter=myGetter)   id property_h; */
/* @property (setter=mySetter:)  id property_i; */

/* Now test various problems.  */

@property (readonly, readwrite) int a;    /* { dg-error ".readonly. attribute conflicts with .readwrite. attribute" } */
/* The following will be enabled when @synthesized is implemented.  */
/* @property (readonly, setter=setA:) int b; */ /* dg-warning ".readonly. attribute conflicts with .setter. attribute" */

@property (assign, retain) id c;          /* { dg-error ".assign. attribute conflicts with .retain. attribute" } */
@property (assign, copy) id d;            /* { dg-error ".assign. attribute conflicts with .copy. attribute" } */
@property (copy, retain) id e;            /* { dg-error ".retain. attribute conflicts with .copy. attribute" } */

/* The following will be enabled when @synthesized is implemented.  */
/* @property (setter=mySetter:,setter=mySetterB:)  id f; */ /* dg-error ".setter. attribute may only be specified once" */
/* @property (getter=myGetter:,getter=myGetterB:)  id f; */ /* dg-error ".getter. attribute may only be specified once" */

@end
