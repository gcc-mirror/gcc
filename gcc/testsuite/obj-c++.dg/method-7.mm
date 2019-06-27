/* Check if sending messages to "underspecified" objects is handled gracefully.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@class UnderSpecified;
typedef struct NotAClass {
  int a, b;
} NotAClass;

void foo(UnderSpecified *u, NotAClass *n) {
  [n nonexistent_method];    /* { dg-warning "invalid receiver type" } */
       /* { dg-warning "no .\\-nonexistent_method. method found" "" { target *-*-* } .-1 } */
  [NotAClass nonexistent_method]; /* { dg-error ".NotAClass. is not an Objective\\-C class name or alias" } */
  [u nonexistent_method];    /* { dg-warning "'.interface' of class .UnderSpecified. not found" } */
                             /* { dg-warning "no .\\-nonexistent_method. method found" "" { target *-*-* } .-1 } */
  [UnderSpecified nonexistent_method]; /* { dg-warning "'.interface' of class .UnderSpecified. not found" } */
                                       /* { dg-warning "no .\\+nonexistent_method. method found" "" { target *-*-* } .-1 } */
}

/* { dg-warning "messages without a matching method signature will be assumed to return .id. and accept .\.\.\.. as arguments" "" { target *-*-* } 0 } */
