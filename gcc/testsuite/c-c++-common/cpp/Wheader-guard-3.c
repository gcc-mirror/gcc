/* PR preprocessor/96842 */
/* { dg-do preprocess } */
/* { dg-options "-Wall" } */

#include "Wheader-guard-3.h"

int i;

/* { dg-warning "header guard 'WHEADER_GUARD_3' followed by '#define' of a different macro" "" { target *-*-* } 0 } */
/* { dg-message "'WHEADERGUARD3' is defined here; did you mean 'WHEADER_GUARD_3'\\\?" "" { target *-*-* } 0 } */
