/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, May 2011.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

#include <objc/objc.h>
#include <stdlib.h>

typedef __SIZE_TYPE__ my_size_t;

@interface MyArray
{
  Class isa;
} 
+ (void) addObject: (id)object __attribute__ ((nonnull));
- (void) addObject: (id)object __attribute__ ((nonnull));

+ (void) insertObject: (id)object  atIndex: (size_t)index __attribute__ ((nonnull (1)));
- (void) insertObject: (id)object  atIndex: (size_t)index __attribute__ ((nonnull (1)));

+ (void) insertObject: (id)object  atIndex: (size_t)index  andObject: (id)anotherObject  atIndex: (size_t)anotherIndex __attribute__ ((nonnull (1, 3)));
- (void) insertObject: (id)object  atIndex: (size_t)index  andObject: (id)anotherObject  atIndex: (size_t)anotherIndex __attribute__ ((nonnull (1, 3)));

/* Test the behavior with invalid code.  */
+ (void) removeObject: (id)object __attribute__ ((nonnull (0))); /* { dg-warning "does not refer to a function parameter" } */
- (void) removeObject: (id)object __attribute__ ((nonnull (0))); /* { dg-warning "does not refer to a function parameter" } */

+ (void) removeObject: (id)object __attribute__ ((nonnull (2))); /* { dg-warning "exceeds the number of function parameters 3" } */
- (void) removeObject: (id)object __attribute__ ((nonnull (2))); /* { dg-warning "exceeds the number of function parameters 3" } */

+ (void) removeObjectAtIndex: (my_size_t)object __attribute__ ((nonnull (1))); /* { dg-warning "refers to parameter type .my_size_t." } */
- (void) removeObjectAtIndex: (my_size_t)object __attribute__ ((nonnull (1))); /* { dg-warning "refers to parameter type .my_size_t." } */

+ (void) removeObject: (id)object __attribute__ ((nonnull (MyArray))); /* { dg-error "" } */
  /* { dg-warning "attribute argument is invalid" "" { target *-*-* } .-1 } */
- (void) removeObject: (id)object __attribute__ ((nonnull (MyArray))); /* { dg-error "" } */
  /* { dg-warning "attribute argument is invalid" "" { target *-*-* } .-1 } */
@end

void test (MyArray *object)
{
  [object addObject: object];
  [object addObject: nil]; /* { dg-warning "\\\[-Wnonnull" } */

  [object insertObject: object atIndex: 4];
  [object insertObject: nil    atIndex: 4]; /* { dg-warning "\\\[-Wnonnull" } */

  [object insertObject: object atIndex: 2 andObject: object atIndex: 3];
  [object insertObject: nil    atIndex: 2 andObject: object atIndex: 3]; /* { dg-warning "\\\[-Wnonnull" } */
  [object insertObject: object atIndex: 2 andObject: nil    atIndex: 3]; /* { dg-warning "\\\[-Wnonnull" } */
}
