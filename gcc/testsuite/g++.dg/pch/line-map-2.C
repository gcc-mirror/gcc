/* PR preprocessor/105608 */
/* { dg-do compile } */
/* { dg-additional-options "-save-temps" } */
#define MACRO_ON_A_LONG_LINE "this line is long enough that it forces the line table to create an LC_RENAME map, which formerly triggered an ICE after PCH restore"
#include "line-map-2.H"
#error "suppress PCH assembly comparison, which does not work with -save-temps" /* { dg-error "." } */
