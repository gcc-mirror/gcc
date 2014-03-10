/* { dg-do compile { target c++11 } } */

typedef double vec __attribute__((vector_size(2*sizeof(double))));
typedef signed char vec2 __attribute__((vector_size(16)));
typedef unsigned char vec2u __attribute__((vector_size(16)));

void f (vec *x, vec *y, vec *z)
{
  *x = (*y < *z) ? *x : *y;
}

void g (vec *x, vec *y, vec *z)
{
  *x = (*y < *z) ? *x : 42;
}

void h (vec *x, vec *y, vec *z)
{
  *x = (*y < *z) ? 3. : *y;
}

void i1 (vec *x, vec *y, vec *z)
{
  auto c = *y < *z;
  *x = c ? *x : *y;
}

void i2 (vec2 *x, vec2 *y, vec2u *z)
{
  *x = *y ? *x : *y;
  *y = *z ? *x : *y;
}

void j (vec2 *x, vec2 *y, vec2 *z, vec *t)
{
  *x = (*y < *z) ? *x : 4.2; /* { dg-error "" } */
  *y = (*x < *z) ? 2.5 : *y; /* { dg-error "" } */
  *t = *t ? *t : *t; /* { dg-error "" } */
  *z = (*x < *z) ? '1' : '0';
}

template <class A, class B>
auto k (A *a, B b) -> decltype (*a ? *a : b);

void k (...) {}

void l (vec2 *v, double x)
{
  k (v, x);
}

