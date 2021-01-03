/* Test for handling of protocol hierarchies.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol NSObj 
- (void)someMethod;
@end

@protocol NSCopying 
- (void)someOtherMethod;
@end

@interface NSObject <NSObj>
- (void)someMethod; 
@end

@implementation NSObject
- (void)someMethod {}
@end

@protocol Booing <NSObj>
- (void)boo;
@end

@interface Boo: NSObject <Booing>  // protocol has only one parent
@end

@implementation Boo
- (void)boo {}
@end

@protocol Fooing <NSCopying, NSObj>  // Fooing has two parent protocols
- (void)foo;
@end

@interface Foo: NSObject <Fooing>
@end

@implementation Foo
- (void)foo {}
- (void)someOtherMethod {}
@end

int foo(void) {
  id<Booing, Fooing> stupidVar;
  [stupidVar boo];
  [stupidVar foo];
  [stupidVar anotherMsg]; /* { dg-warning ".\\-anotherMsg. not found in protocol" } */
       /* { dg-warning "no .\\-anotherMsg. method found" "" { target *-*-* } .-1 } */
  return 0;
}

/* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */
