/* { dg-do compile } */
typedef float v2sf __attribute__ ((vector_size (8)));
v2sf sub (void) { return (v2sf) 0.0; } /* { dg-error "can't convert" } */
