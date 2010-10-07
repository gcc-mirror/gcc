/* Test for situations in which protocol conformance information
   may be lost while casting.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

#include <stdint.h>

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol Proto
- (intptr_t)someValue;
@end

@interface Obj
- (intptr_t)anotherValue;
@end

long foo(void) {
  intptr_t receiver = 2;
  Obj *objrcvr;
  Obj <Proto> *objrcvr2;

  /* NB: Since 'receiver' is an invalid ObjC message receiver, the compiler
     should warn but then search for methods as if we were messaging 'id'.  */

  receiver += [receiver someValue]; /* { dg-warning "invalid receiver type .intptr_t." } */
  receiver += [receiver anotherValue]; /* { dg-warning "invalid receiver type .intptr_t." } */

  receiver += [(Obj *)receiver someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 30 } */

  receiver += [(Obj *)receiver anotherValue];
  receiver += [(Obj <Proto> *)receiver someValue];
  receiver += [(Obj <Proto> *)receiver anotherValue];
  receiver += [objrcvr someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 36 } */

  receiver += [objrcvr anotherValue];
  receiver += [(Obj <Proto> *)objrcvr someValue];
  receiver += [(Obj <Proto> *)objrcvr anotherValue];
  receiver += [objrcvr2 someValue];
  receiver += [objrcvr2 anotherValue];
  receiver += [(Obj *)objrcvr2 someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 44 } */

  receiver += [(Obj *)objrcvr2 anotherValue];

  return receiver;
}

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */
