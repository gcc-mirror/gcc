// { dg-do run { target c++17_down } }
// { dg-options "" }

const unsigned char r[64] = {
#embed __FILE__ limit (64)
};
struct S { int a; long b; unsigned char c[63]; int d; };
S s = {
#embed __FILE__ limit (64) prefix (.a = 1, .b = ) suffix (, .d = 2)
};
const unsigned char t[66] = {
#embed __FILE__ limit (64) prefix ([0] = 1, [1] =) suffix (, [65] = 2)
};
int u[] = { [0] =
#embed __FILE__ limit (64)
};

int
main ()
{
  if (s.a != 1 || s.b != r[0] || __builtin_memcmp (s.c, r + 1, 63) || s.d != 2)
    __builtin_abort ();
  if (t[0] != 1 || __builtin_memcmp (t + 1, r, 64) || t[65] != 2)
    __builtin_abort ();
  for (int i = 0; i < 64; ++i)
    if (u[i] != r[i])
      __builtin_abort ();
}
