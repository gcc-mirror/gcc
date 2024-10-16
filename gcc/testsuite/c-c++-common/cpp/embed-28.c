/* { dg-do run } */
/* { dg-options "--embed-directory ${srcdir}/c-c++-common/cpp/embed-dir" } */
/* { dg-additional-options "-std=gnu99" { target c } } */

const unsigned char magna_carta[] = {
#embed <magna-carta.txt> limit (256)
};

typedef unsigned char V __attribute__((vector_size (256)));
#define TEN(x) x##0, x##1, x##2, x##3, x##4, x##5, x##6, x##7, x##8, x##9

int
main ()
{
  if (__CHAR_BIT__ != 8)
    return 0;
  V a = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
          TEN (1), TEN (2), TEN (3), TEN (4),
          TEN (5), TEN (6), TEN (7), TEN (8),
          TEN (9), TEN (10), TEN (11), TEN (12),
          TEN (13), TEN (14), TEN (15), TEN (16),
          TEN (17), TEN (18), TEN (19), TEN (20),
          TEN (21), TEN (22), TEN (23), TEN (24),
          250, 251, 252, 253, 254, 255 };
  V b = __builtin_shufflevector (a, a,
#embed <magna-carta.txt> limit (256)
                                );
  V c = __builtin_shufflevector (b, b,
#embed <magna-carta.txt> limit (256)
                                );
  int i;
  for (i = 0; i < 256; ++i)
    if (b[i] != magna_carta[i])
      __builtin_abort ();
  for (i = 0; i < 256; ++i)
    if (c[i] != magna_carta[magna_carta[i]])
      __builtin_abort ();
}
