/* { dg-do compile } */
/* { dg-options "" } */
struct __attribute__ ((visibility("default"))) Foo { int foo; }; /* { dg-warning "attribute ignored on types" } */
