/* Test for situations in which protocol conformance information
   may be lost while casting.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol Proto
- (long)someValue;
@end

@interface Obj
- (long)anotherValue;
@end

long foo(void) {
  long receiver = 2;
  Obj *objrcvr;
  Obj <Proto> *objrcvr2;

  /* NB: Since 'receiver' is an invalid ObjC message receiver, the compiler
     should warn but then search for methods as if we were messaging 'id'.  */

  receiver += [receiver someValue]; /* { dg-warning "invalid receiver type .long int." } */
  receiver += [receiver anotherValue]; /* { dg-warning "invalid receiver type .long int." } */

  receiver += [(Obj *)receiver someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-error "invalid conversion" "" { target *-*-* } 28 } */

  receiver += [(Obj *)receiver anotherValue];
  receiver += [(Obj <Proto> *)receiver someValue];
  receiver += [(Obj <Proto> *)receiver anotherValue];
  receiver += [objrcvr someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-error "invalid conversion" "" { target *-*-* } 34 } */

  receiver += [objrcvr anotherValue];
  receiver += [(Obj <Proto> *)objrcvr someValue];
  receiver += [(Obj <Proto> *)objrcvr anotherValue];
  receiver += [objrcvr2 someValue];
  receiver += [objrcvr2 anotherValue];
  receiver += [(Obj *)objrcvr2 someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "invalid conversion" "" { target *-*-* } 42 } */

  receiver += [(Obj *)objrcvr2 anotherValue];

  return receiver;
}

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */
