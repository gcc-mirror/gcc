/* Test out static (non-heap) allocations of ObjC class instances.
   These should elicit errors.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface Object {
  struct objc_class *isa;
}
@end

@compatibility_alias MyObject Object;

@interface Foo: Object {
  int a;
  Object *b;
  Object c; /* { dg-error "statically allocated instance of Objective-C class .Object." } */
}
@end

@compatibility_alias MyFoo Foo;

typedef Foo FooAlias1;
typedef FooAlias1 FooAlias2;
typedef Object ObjectAlias1;
typedef struct Object ObjectAlias2;
Object staticObject1;   /* { dg-error "statically allocated instance of Objective-C class .Object." } */
struct Object staticObject2;   /* { dg-error "statically allocated instance of Objective-C class .Object." } */
static ObjectAlias1 staticObject3;   /* { dg-error "statically allocated instance of Objective-C class .Object." } */
FooAlias1 staticFoo1;  /* { dg-error "statically allocated instance of Objective-C class .Foo." } */
extern FooAlias2 externFoo1;  /* { dg-error "statically allocated instance of Objective-C class .Foo." } */
static Foo staticFoo2;  /* { dg-error "statically allocated instance of Objective-C class .Foo." } */
MyObject staticMyObject1;  /* { dg-error "statically allocated instance of Objective-C class .Object." } */
MyFoo staticMyFoo1;  /* { dg-error "statically allocated instance of Objective-C class .Foo." } */
