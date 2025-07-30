/* PR preprocessor/105608 */
/* { dg-do compile } */
#define INT int /* { dg-error "-:declaration does not declare anything" } */
#include "line-map-4.H"
INT; /* { dg-note "in expansion of macro" } */
