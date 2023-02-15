/* Test for situations in which protocol conformance information
   may be lost while casting.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

typedef __UINTPTR_TYPE__ uintptr_t;

@protocol Proto
- (uintptr_t)someValue;
@end

@interface Obj
- (uintptr_t)anotherValue;
@end

uintptr_t foo(void) {
  uintptr_t receiver = 2;
  Obj *objrcvr;
  Obj <Proto> *objrcvr2;

  /* NB: Since 'receiver' is an invalid ObjC message receiver, the compiler
     should warn but then search for methods as if we were messaging 'id'.  */

  receiver += [receiver someValue]; /* { dg-warning "invalid receiver type .uintptr_t." } */
  receiver += [receiver anotherValue]; /* { dg-warning "invalid receiver type .uintptr_t." } */

  receiver += [(Obj *)receiver someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */

  receiver += [(Obj *)receiver anotherValue];
  receiver += [(Obj <Proto> *)receiver someValue];
  receiver += [(Obj <Proto> *)receiver anotherValue];
  receiver += [objrcvr someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */

  receiver += [objrcvr anotherValue];
  receiver += [(Obj <Proto> *)objrcvr someValue];
  receiver += [(Obj <Proto> *)objrcvr anotherValue];
  receiver += [objrcvr2 someValue];
  receiver += [objrcvr2 anotherValue];
  receiver += [(Obj *)objrcvr2 someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */

  receiver += [(Obj *)objrcvr2 anotherValue];

  return receiver;
}

/* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */
