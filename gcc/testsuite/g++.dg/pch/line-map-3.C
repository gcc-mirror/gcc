/* PR preprocessor/105608 */
/* { dg-do compile } */
/* { dg-additional-options "-Wunused-macros" } */
#define UNUSED_MACRO /* { dg-warning "-:UNUSED_MACRO" "" } */
#include "line-map-3.H" /* { dg-bogus "-:UNUSED_MACRO" "" } */
