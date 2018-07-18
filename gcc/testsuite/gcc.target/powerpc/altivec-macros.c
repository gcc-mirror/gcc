/* Copyright (C) 2007 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

/* Conditional macros should not be expanded by pragmas.  */
#pragma __vector
_Pragma ("__vector")

/* Redefinition of conditional macros.  */
/* No warning should be generated.  */

#define __vector __new_vector
#define __pixel __new_pixel
#define __bool __new_bool
#define vector new_vector
#define pixel new_pixel
#define bool new_bool

/* Definition of conditional macros.  */
/* No warning should be generated.  */

#undef __vector
#define __vector __new_vector
/* { dg-message "-:location of the previous"  "prev __vector defn"  { target *-*-* } .-1 } */

#undef __pixel
#define __pixel __new_pixel
/* { dg-message "-:location of the previous"  "prev __pixel defn"   { target *-*-* } .-1 } */

#undef __bool
#define __bool __new_bool
/* { dg-message "-:location of the previous"  "prev __bool defn"    { target *-*-* } .-1 } */

#undef vector
#define vector new_vector
/* { dg-message "-:location of the previous"  "prev vector defn"    { target *-*-* } .-1 } */

#undef pixel
#define pixel new_pixel
/* { dg-message "-:location of the previous"  "prev pixel defn"     { target *-*-* } .-1 } */

#undef bool
#define bool new_bool
/* { dg-message "-:location of the previous"  "prev bool defn"      { target *-*-* } .-1 } */

/* Re-definition of "unconditional" macros.  */
/* Warnings should be generated as usual.  */

#define __vector	__newer_vector
/* { dg-warning "-:redefined" "__vector redefined"  { target *-*-* } .-1 } */

#define __pixel		__newer_pixel
/* { dg-warning "-:redefined" "__pixel redefined"   { target *-*-* } .-1 } */

#define __bool		__newer_bool
/* { dg-warning "-:redefined" "__bool redefined"    { target *-*-* } .-1 } */

#define vector		newer_vector
/* { dg-warning "-:redefined" "vector redefined"    { target *-*-* } .-1 } */

#define pixel		newer_pixel
/* { dg-warning "-:redefined" "pixel redefined"     { target *-*-* } .-1 } */

#define bool		newer_bool
/* { dg-warning "-:redefined" "bool redefined"      { target *-*-* } .-1 } */
