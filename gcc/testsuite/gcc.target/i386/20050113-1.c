/* PR middle-end/19164 */
/* { dg-do compile } */
/* { dg-options "-mmmx" } */

typedef short int V __attribute__ ((vector_size (8)));
static V v = (V) 0x00FF00FF00FF00FFLL;
