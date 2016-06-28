/* PR c/71552 - Confusing error for incorrect struct initialization */
/* { dg-do compile } */

struct A { void *p; };
struct B { struct A *p; };
struct A a;

/* Verify that the initializer is diagnosed for its incompatibility
   with the type of the object being initialized, not for its lack
   of constness (which is a lesser problem).  */
struct B b = { a };   /* { dg-error "incompatible types when initializing" } */
struct B *p = a;      /* { dg-error "incompatible types when initializing" } */
