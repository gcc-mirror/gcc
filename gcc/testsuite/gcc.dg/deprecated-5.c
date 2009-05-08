/* Test __attribute__((deprecated)).  Test types without names.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "" } */

struct { int a; } __attribute__((deprecated ("Do not use"))) x; /* { dg-warning "type is deprecated" } */
typeof(x) y; /* { dg-warning "type is deprecated .declared at .*.: Do not use" } */
