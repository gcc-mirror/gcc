/* REPRODUCED:CC1:SIGNAL MACHINE:m68k OPTIONS:-fpcc-struct-return */
struct b{};
f(struct b(*f)())
{
struct b d=f();
}

