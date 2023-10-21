/* REPRODUCED:CC1:SIGNAL MACHINE:m68k OPTIONS:-fpcc-struct-return */

/* { dg-require-effective-target indirect_calls } */
/* { dg-additional-options "-std=gnu89" } */

struct b{};
f(struct b(*f)())
{
struct b d=f();
}

