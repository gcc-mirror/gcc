/* Don't ICE or emit spurious errors when init a vector with a scalar.  */
/* { dg-do compile } */
typedef float v2sf __attribute__ ((vector_size (8)));
v2sf a = 0.0;  /* { dg-error "incompatible types|cannot convert" } */
