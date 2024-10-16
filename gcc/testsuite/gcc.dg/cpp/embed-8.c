/* { dg-do run } */
/* { dg-options "-std=c23" } */

unsigned char a[] = {
#embed __FILE__
};
unsigned char b[] = {
  [26] =
#embed __FILE__
};
unsigned char c[] = {
#embed __FILE__ suffix (,)
  [sizeof (a) / 4] = 0,
  [sizeof (a) / 2] = 1,
  [1] = 2,
  [sizeof (a) - 2] = 3
};
unsigned char d[] = {
  [1] = 4,
  [26] = 5,
  [sizeof (a) / 4] = 6,
  [sizeof (a) / 2] = 7,
  [sizeof (a) - 2] = 8,
#embed __FILE__ prefix ([0] = )
};
unsigned char e[] = {
#embed __FILE__ suffix (,)
  [2] = 9,
  [sizeof (a) - 3] = 10
};
unsigned char f[] = {
  [23] = 11,
  [sizeof (a) / 4 - 1] = 12,
#embed __FILE__ limit (128) prefix ([sizeof (a) / 4 - 1] = ) suffix (,)
#embed __FILE__ limit (130) prefix ([sizeof (a) / 4 - 2] = ) suffix (,)
#embed __FILE__ prefix ([sizeof (a) / 4 + 10] = ) suffix (,)
#embed __FILE__ limit (128) prefix ([sizeof (a) + sizeof (a) / 4 - 30] = ) suffix (,)
#embed __FILE__ limit (128) prefix ([sizeof (a) / 4 + 96] = ) suffix (,)
};
const unsigned char g[] = {
#embed __FILE__ limit (128) prefix (  [10] = 2, [5] = 3, [13] = 4, [17] = 5, [0] = )
};
unsigned char z[sizeof (a) / 4] = {
};

int
main ()
{
  if (sizeof (b) != sizeof (a) + 26
      || __builtin_memcmp (a, b + 26, sizeof (a)))
    __builtin_abort ();
  if (sizeof (c) != sizeof (a)
      || a[0] != c[0]
      || c[1] != 2
      || __builtin_memcmp (a + 2, c + 2, sizeof (a) / 4 - 2)
      || c[sizeof (a) / 4] != 0
      || __builtin_memcmp (a + sizeof (a) / 4 + 1, c + sizeof (a) / 4 + 1, sizeof (a) / 2 - sizeof (a) / 4 - 1)
      || c[sizeof (a) / 2] != 1
      || __builtin_memcmp (a + sizeof (a) / 2 + 1, c + sizeof (a) / 2 + 1, sizeof (a) - sizeof (a) / 2 - 3)
      || c[sizeof (a) - 2] != 3
      || a[sizeof (a) - 1] != c[sizeof (a) - 1])
    __builtin_abort ();
  if (sizeof (d) != sizeof (a)
      || __builtin_memcmp (a, d, sizeof (a)))
    __builtin_abort ();
  if (sizeof (e) != sizeof (a)
      || a[0] != e[0]
      || a[1] != e[1]
      || e[2] != 9
      || __builtin_memcmp (a + 3, e + 3, sizeof (a) - 6)
      || e[sizeof (a) - 3] != 10
      || a[sizeof (a) - 2] != e[sizeof (a) - 2]
      || a[sizeof (a) - 1] != e[sizeof (a) - 1])
    __builtin_abort ();
  if (sizeof (f) != sizeof (a) + sizeof (a) / 4 - 30 + 128
      || __builtin_memcmp (z, f, 23)
      || f[23] != 11
      || __builtin_memcmp (z, f + 24, sizeof (a) / 4 - 2 - 24)
      || __builtin_memcmp (f + sizeof (a) / 4 - 2, a, 12)
      || __builtin_memcmp (f + sizeof (a) / 4 + 10, a, 86)
      || __builtin_memcmp (f + sizeof (a) / 4 + 96, a, 128)
      || __builtin_memcmp (f + sizeof (a) / 4 + 96 + 128, a + 86 + 128, sizeof (a) - 86 - 128 - 40)
      || __builtin_memcmp (f + sizeof (a) + sizeof (a) / 4 - 30, a, 128))
    __builtin_abort ();
  if (sizeof (g) != 128 || __builtin_memcmp (g, a, 128))
    __builtin_abort ();
}
