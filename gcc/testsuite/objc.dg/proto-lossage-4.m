/* Test for situations in which protocol conformance information
   may be lost while casting.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

/* One-line substitute for objc/objc.h */
typedef struct objc_object { struct objc_class *class_pointer; } *id;

@protocol Proto
- (int)someValue;
@end

@interface Obj
- (int)anotherValue;
@end

int foo(void) {
  int receiver = 2;
  Obj *objrcvr;
  Obj <Proto> *objrcvr2;

  receiver += [receiver someValue]; /* { dg-warning "invalid receiver type .int( )?." } */
/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 22 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 22 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 22 } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 22 } */

  receiver += [receiver anotherValue]; /* { dg-warning "invalid receiver type .int( )?." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 28 } */
  
  receiver += [(Obj *)receiver someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 31 } */

  receiver += [(Obj *)receiver anotherValue];
  receiver += [(Obj <Proto> *)receiver someValue];
  receiver += [(Obj <Proto> *)receiver anotherValue];
  receiver += [objrcvr someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 37 } */

  receiver += [objrcvr anotherValue];
  receiver += [(Obj <Proto> *)objrcvr someValue];
  receiver += [(Obj <Proto> *)objrcvr anotherValue];
  receiver += [objrcvr2 someValue];
  receiver += [objrcvr2 anotherValue];
  receiver += [(Obj *)objrcvr2 someValue]; /* { dg-warning ".Obj. may not respond to .\\-someValue." } */
/* { dg-warning "assignment makes integer from pointer without a cast" "" { target *-*-* } 45 } */

  receiver += [(Obj *)objrcvr2 anotherValue];

  return receiver;
}
