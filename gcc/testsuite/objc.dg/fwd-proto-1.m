/* Test forward-decls for @protocols.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol Bar;
@protocol Boo;

@protocol Foo 
- (id <Bar>)someMethod;
- (id <Baz>)anotherMethod; /* { dg-error "annot find protocol declaration" } */
@end

@protocol Bar <Boo>
- (id <Foo>)someOtherMethod;
- (id <Baz>)anotherMethod; /* { dg-error "annot find protocol declaration" } */
- (id <Boo>)yetAnotherMethod;
@end

/* The following worthy test is stubbed out until we can get the
   harness to match correctly on the "compilation terminated" message
   when running on GNU/Linux.  sts 2001-08-01 */
#if 0
@protocol Boo <Bar>   /* { /*dg*/-error "has circular dependency" } */
@end
#endif

