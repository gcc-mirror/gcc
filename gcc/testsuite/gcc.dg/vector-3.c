/* { dg-do compile } */

/* Check that we error out when using vector_size on the bool type. */

__attribute__((vector_size(16) )) _Bool a; /* { dg-error "" } */
