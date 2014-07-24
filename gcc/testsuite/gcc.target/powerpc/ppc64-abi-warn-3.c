/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

struct test
  {
    int a __attribute__((vector_size (8)));
  }; /* { dg-message "note: the layout of aggregates containing vectors with 8-byte alignment will change" } */

