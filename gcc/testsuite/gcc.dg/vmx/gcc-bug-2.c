/* { dg-do compile } */
#include <altivec.h>
typedef struct n_a {
  signed char m1;
  vector float m2;
} n_a;

typedef struct n_b {
  signed char m1;
  struct n_a m2;
} n_b;

extern void f(n_b *);

void initn_b()
{
   n_b _i;
  f(&_i);
}
