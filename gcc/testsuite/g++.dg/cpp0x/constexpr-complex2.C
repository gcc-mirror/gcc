// { dg-do compile { target c++11 } }
// { dg-options "" }

static _Complex int i;
static_assert (&__imag i == &__imag i, "");
