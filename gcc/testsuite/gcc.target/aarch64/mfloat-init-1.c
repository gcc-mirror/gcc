/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */

/* { dg-error "invalid conversion to type 'mfloat8_t" "" {target *-*-*} 0 } */
__Mfloat8x8_t const_mf8x8 () { return (__Mfloat8x8_t) { 1, 1, 1, 1, 1, 1, 1, 1 }; }
