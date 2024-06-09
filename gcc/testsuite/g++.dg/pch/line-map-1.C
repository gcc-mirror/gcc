/* PR preprocessor/105608 */
/* { dg-do compile } */
#define MACRO_ON_A_LONG_LINE "this line is long enough that it forces the line table to create an LC_RENAME map, which formerly triggered an ICE after PCH restore"
#include "line-map-1.H"
