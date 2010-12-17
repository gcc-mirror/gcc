// { dg-do compile }
// Contributed by: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
#include <cstddef>

template <int  T> struct A {};
template <void* T> struct B {};

A<NULL> a;			// { dg-warning "NULL" }
B<NULL> b;  // { dg-error "" }

