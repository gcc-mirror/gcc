/* Check if sending messages to "underspecified" objects is handled gracefully.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@class UnderSpecified;
typedef struct NotAClass {
  int a, b;
} NotAClass;

void foo(UnderSpecified *u, NotAClass *n) {
  [n nonexistent_method];    /* { dg-warning "invalid receiver type" } */
       /* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 11 } */
       /* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 11 } */
       /* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 11 } */
  [NotAClass nonexistent_method]; /* { dg-error ".NotAClass. is not an Objective\\-C class name or alias" } */
  [u nonexistent_method]; /* { dg-warning ".UnderSpecified. may not respond to .\\-nonexistent_method." } */
  [UnderSpecified nonexistent_method]; /* { dg-warning ".UnderSpecified. may not respond to .\\+nonexistent_method." } */
}
