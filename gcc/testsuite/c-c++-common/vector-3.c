/* { dg-do compile } */

/* Check that we error out when using vector_size on the bool type. */

#ifdef __cplusplus
#define _Bool bool
#endif
__attribute__((vector_size(16) )) _Bool a; /* { dg-error "" } */
