/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that the compiler can correctly compare protocols in types of
   method signatures.  */

@protocol A, B, C;

@interface MyClass
- (void) method1: (id)x;
- (void) method1: (id)x; /* Ok */

- (void) method2: (id <A>)x;
- (void) method2: (id <A>)x; /* Ok */

- (void) method3: (id <A, B>)x;
- (void) method3: (id <A, B>)x; /* Ok */

- (void) method4: (id <A, B>)x;
- (void) method4: (id <A, B, B>)x; /* Ok */

- (void) method5: (id <A, A, B>)x;
- (void) method5: (id <A, B, B>)x; /* Ok */

- (void) method6: (id <A, A, B, B, C, C>)x;
- (void) method6: (id <C, A, B>)x; /* Ok */

- (void) method7: (id)x; /* { dg-message "previous declaration" } */
- (void) method7: (id <A>)x; /* { dg-error "duplicate declaration" } */

- (void) method8: (id <A>)x; /* { dg-message "previous declaration" } */
- (void) method8: (id)x; /* { dg-error "duplicate declaration" } */

- (void) method9: (id <A>)x; /* { dg-message "previous declaration" } */
- (void) method9: (id <B>)x; /* { dg-error "duplicate declaration" } */

- (void) methodA: (id <A>)x; /* { dg-message "previous declaration" } */
- (void) methodA: (id <A, B>)x; /* { dg-error "duplicate declaration" } */

- (void) methodB: (id <A, B>)x; /* { dg-message "previous declaration" } */
- (void) methodB: (id <A>)x; /* { dg-error "duplicate declaration" } */

- (void) methodC: (id <A, B, C>)x; /* { dg-message "previous declaration" } */
- (void) methodC: (id <A, B>)x; /* { dg-error "duplicate declaration" } */

- (void) methodD: (id <A, B, C>)x; /* { dg-message "previous declaration" } */
- (void) methodD: (id <A, B, A>)x; /* { dg-error "duplicate declaration" } */

- (void) methodE: (MyClass <A, B, C> *)x; /* { dg-message "previous declaration" } */
- (void) methodE: (MyClass <A, B, A> *)x; /* { dg-error "duplicate declaration" } */

- (void) methodF: (MyClass <A, B, A> *)x;
- (void) methodF: (MyClass <A, B, A> *)x; /* Ok */

- (void) methodG: (MyClass *)x;  /* { dg-message "previous declaration" } */
- (void) methodG: (MyClass <A, B, C> *)x; /* { dg-error "duplicate declaration" } */

- (void) methodH: (MyClass <A, C>*)x;  /* { dg-message "previous declaration" } */
- (void) methodH: (MyClass *)x; /* { dg-error "duplicate declaration" } */

@end
