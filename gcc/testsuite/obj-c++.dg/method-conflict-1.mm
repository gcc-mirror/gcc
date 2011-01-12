/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that you can not declare two methods, in the same protocol,
   with the same name but conflicting method signatures.  */

@protocol MyProtocol
+ (int) method1: (int)x;   /* { dg-message "previous declaration" } */
+ (float) method1: (int)x; /* { dg-error "duplicate declaration of method .\\+method1." } */

- (int) method2: (int)x;   /* { dg-message "previous declaration" } */
- (int) method2: (float)x; /* { dg-error "duplicate declaration of method .\\-method2." } */

@optional
+ (int *) method3: (int)x;    /* { dg-message "previous declaration" } */
+ (int *) method3: (int **)x; /* { dg-error "duplicate declaration of method .\\+method3." } */

- (id) method4: (id)x;   /* { dg-message "previous declaration" } */
- (void) method4: (id)x; /* { dg-error "duplicate declaration of method .\\-method4." } */
@end

/* We don't test conflicting types between @required and @optional
   methods, as that is tested in method-conflict-2.  */

