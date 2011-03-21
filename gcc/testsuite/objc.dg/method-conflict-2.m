/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that you can not declare two methods, in the same protocol,
   with the same name and method signature, but one as @required and
   once as @optional.  */

/* First, @required conflicting with @optional.  */
@protocol MyProtocol

@optional
+ (void) method1: (id)x; /* { dg-message "previous declaration" } */
- (id) method2: (long)x; /* { dg-message "previous declaration" } */

@required
+ (void) method1: (id)x; /* { dg-error "declared .@optional. and .@required. at the same time" } */
- (id) method2: (long)x; /* { dg-error "declared .@optional. and .@required. at the same time" } */

@end

/* Second, @optional conflicting with @required.  */
@protocol MyProtocol2

@required
+ (void) method3: (Class)x; /* { dg-message "previous declaration" } */
- (id *) method4: (long)x;  /* { dg-message "previous declaration" } */

@optional
+ (void) method3: (Class)x; /* { dg-error "declared .@optional. and .@required. at the same time" } */
- (id *) method4: (long)x;  /* { dg-error "declared .@optional. and .@required. at the same time" } */

@end
