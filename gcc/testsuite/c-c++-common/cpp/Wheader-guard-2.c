/* PR preprocessor/96842 */
/* { dg-do preprocess } */
/* { dg-options "-Wheader-guard" } */

#include "Wheader-guard-2.h"

int i;

/* { dg-warning "header guard 'WHEADER_GUARD_2' followed by '#define' of a different macro" "" { target *-*-* } 0 } */
/* { dg-message "'WHEADERGUARD2' is defined here; did you mean 'WHEADER_GUARD_2'\\\?" "" { target *-*-* } 0 } */
