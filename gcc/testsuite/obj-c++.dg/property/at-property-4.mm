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
@property (assign)    id property_as_1;
@property (copy)      id property_as_2;
@property (retain)    id property_as_3;

@property (atomic)    int property_at_1;
@property (nonatomic) int property_at_2;

@property (readonly)  int property_rw_1;
@property (readwrite) int property_rw_2;

@property (class) int property_cl_1;

@property (null_unspecified) int *property_null_1;
@property (nullable) int *property_null_2;
@property (nonnull) int *property_null_3;
@property (null_resettable) int *property_null_4;

@property (release)   int property_err_1;      /* { dg-error "unknown property attribute" } */

@property (getter=myGetter)  int property_g0;
@property (setter=mySetter:) int property_s0;

/* Now test various basic problems.  */

@property (readonly, readwrite) int a;    /* { dg-error ".readwrite. attribute conflicts with .readonly. attribute" } */
@property (readonly, setter=mySetterB:) int b; /* { dg-error ".readonly. attribute conflicts with .setter. attribute" } */

@property (assign, retain) id c;          /* { dg-error ".retain. attribute conflicts with .assign. attribute" } */
@property (assign, copy) id d;            /* { dg-error ".copy. attribute conflicts with .assign. attribute" } */
@property (copy, retain) id e;            /* { dg-error ".retain. attribute conflicts with .copy. attribute" } */

@property (atomic, nonatomic) int property_j; /* { dg-error {'nonatomic' attribute conflicts with 'atomic' attribute} } */

@property (null_unspecified) int property_bad_t_1; /* { dg-error {nullability specifier 'null_unspecified' cannot be applied to non-pointer type 'int'} } */
@property (nullable) int property_bad_t_2;/* { dg-error {nullability specifier 'nullable' cannot be applied to non-pointer type 'int'} } */
@property (nonnull) int property_bad_t_3;/* { dg-error {nullability specifier 'nonnull' cannot be applied to non-pointer type 'int'} } */
@property (null_resettable) int property_bad_t_4;/* { dg-error {nullability specifier 'null_resettable' cannot be applied to non-pointer type 'int'} } */
@property (nullable) int **property_bad_t_5;/* { dg-error {nullability specifier 'nullable' cannot be applied to multi-level pointer type 'int\*\*'} } */

@property (null_unspecified, nullable) int *property_ne_1; /* { dg-error {'nullable' attribute conflicts with 'null_unspecified' attribute} } */
@property (null_unspecified, nonnull) int *property_ne_2; /* { dg-error {'nonnull' attribute conflicts with 'null_unspecified' attribute} } */
@property (null_unspecified, null_resettable) int *property_ne_3; /* { dg-error {'null_resettable' attribute conflicts with 'null_unspecified' attribute} } */
@property (nullable,nonnull) int *property_ne_4; /* { dg-error {'nonnull' attribute conflicts with 'nullable' attribute} } */
@property (nullable,null_resettable) int *property_ne_5; /* { dg-error {'null_resettable' attribute conflicts with 'nullable' attribute} } */
@property (nonnull, null_resettable) int *property_ne_6; /* { dg-error {'null_resettable' attribute conflicts with 'nonnull' attribute} } */

@property (setter=mySetter:,setter=mySetter2:)  int f; /* { dg-warning {multiple property 'setter' methods specified, the latest one will be used} } */
@property (getter=myGetter, getter=myGetter2 )  int g; /* { dg-warning {multiple property 'getter' methods specified, the latest one will be used} } */

@end
