// PR c++/102538
// { dg-do run { target c++20 } }

struct X { union { char r8[8]; int r32[2]; }; };
struct Y { X v[1]; };
Y x = { { { .r32 = { 5, 6 } } } };

int
main ()
{
  if (x.v[0].r32[0] != 5 || x.v[0].r32[1] != 6)
    __builtin_abort ();
}
