/* Test __attribute__((unavailable)).  Test types without names.  */
/* { dg-do compile } */
/* { dg-options "" } */

struct { int a; } __attribute__((unavailable ("Do not use"))) x; /* { dg-error "type is unavailable" } */
typeof(x) y; /* { dg-error "type is unavailable: Do not use" } */
