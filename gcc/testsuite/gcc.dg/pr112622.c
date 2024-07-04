/* { dg-do compile } */

typedef __attribute__((__vector_size__(4))) float V;

V v = (float)-v; /* { dg-error "vector value used" } */
