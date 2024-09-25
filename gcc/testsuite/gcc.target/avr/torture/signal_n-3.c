/* { dg-do compile } */
/* { dg-additional-options "-Wattributes" } */

__attribute__((signal(0))) static void fun1 (void); /* { dg-error "expects a constant positive integer" } */

__attribute__((signal("1"))) static void fun2 (void); /* { dg-error "expects a constant positive integer" } */

__attribute__((interrupt(-1))) void fun3 (void); /* { dg-error "expects a constant positive integer" } */

__attribute__((interrupt("2"))) void fun4 (void); /* { dg-error "expects a constant positive integer" } */

__attribute__((noblock)) void fun5 (void) { } /* { dg-warning "attribute ignored on non-ISR" } */

