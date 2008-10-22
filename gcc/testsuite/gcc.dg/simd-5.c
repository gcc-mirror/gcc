/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu89" } */

/* Ensure that we don't need a typedef to initialize a vector type.  */
#define vector __attribute__ ((vector_size (8)))
vector char x = (vector char) {1,2,3,4,5,6,7,8}; /* { dg-bogus "initializer" } */
vector char y = (vector short) {1,2,3,4}; /* { dg-message "note: use -flax-vector-conversions to permit conversions between vectors with differing element types or numbers of subparts" } */
  /* { dg-error "incompatible types when initializing" "" { target *-*-* } 7 } */
