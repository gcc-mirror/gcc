/* { dg-do compile } */

#include <arm_sve.h>

template<typename T>
struct S
{
  S(T);
  operator T() const;
  void *base;
};

void f(svbool_t pg, const S<svuint8_t> &u8a, const S<svuint8_t> &u8b,
       const S<svint8_t> &s8a)
{
  svadd_x(pg, u8a, u8b);
  svadd_x(pg, u8a, 1);
  svadd_x(pg, s8a, u8b); // { dg-error "no matching function for call" }
  svadd_x(pg, s8a, 1);
}
