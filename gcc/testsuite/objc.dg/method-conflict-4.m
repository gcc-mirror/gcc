/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that the compiler can correctly compare protocols in types of
   method signatures.  In this test we look at protocols implementing
   other protocols.  The fact that one protocol implements another one
   doesn't mean that they are identical.  */

@protocol A
- (void) doSomething;
@end

@protocol B <A>
- (void) doSomethingElse;
@end

@protocol C <A>
- (void) doYetSomethingElse;
@end

@interface MyClass2
- (void) aMethod: (id <A>)x;  /* { dg-message "previous declaration" } */
- (void) aMethod: (id <B>)x;  /* { dg-error "duplicate declaration" } */

- (void) bMethod: (id <B>)x;  /* { dg-message "previous declaration" } */
- (void) bMethod: (id <A>)x;  /* { dg-error "duplicate declaration" } */

- (void) cMethod: (id <A, B>)x;
- (void) cMethod: (id <B>)x;  /* Ok - because if you implement B, then you also implement A, so <B> == <A, B> */

- (void) dMethod: (id <A, B>)x;
- (void) dMethod: (id <B, A>)x; /* Ok */

- (void) eMethod: (id <A>)x;  /* { dg-message "previous declaration" } */
- (void) eMethod: (id <B, C>)x;  /* { dg-error "duplicate declaration" } */

- (void) fMethod: (id <B, C>)x;  /* { dg-message "previous declaration" } */
- (void) fMethod: (id <A>)x;  /* { dg-error "duplicate declaration" } */

- (void) gMethod: (id <A>)x;  /* { dg-message "previous declaration" } */
- (void) gMethod: (id <A, B, C>)x;  /* { dg-error "duplicate declaration" } */

- (void) hMethod: (id <A, B, C>)x;  /* { dg-message "previous declaration" } */
- (void) hMethod: (id <A>)x;  /* { dg-error "duplicate declaration" } */
@end
