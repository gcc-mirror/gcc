// PR c++/121678
// { dg-do compile { target c++11 } }
// { dg-options "" }

static constexpr _Complex double a = 1.0;
static constexpr double *r = &__real__ a;
static constexpr double *i = &__imag__ a;
