/* PR c++/121678 */
/* { dg-do compile } */
/* { dg-options "" } */

static const _Complex double a = 1.0;
static const double *const r = &__real__ a;
static const double *const i = &__imag__ a;
