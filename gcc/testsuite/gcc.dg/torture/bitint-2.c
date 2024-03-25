/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

__attribute__((noipa)) void
foo (unsigned _BitInt(25) a, unsigned _BitInt(27) b, unsigned _BitInt(25) *p, unsigned _BitInt(27) *q, float c)
{
  p[0] = b;
  q[0] = a;
  q[1] = (signed _BitInt(25)) a;
  q[2] = (_BitInt(12)) a;
  q[3] = c;
  q[4] += a;
  q[5] = a + b;
  q[6] = a - b;
  q[7] = a * b;
  q[8] = a / b;
  q[9] = a % b;
  q[10] = b << (24320393uwb - a);
  q[11] = (b * 131uwb) >> (24320393uwb - a);
  q[12]++;
  ++q[13];
  q[14]--;
  --q[15];
  q[16] = a == b;
  q[17] = a != b;
  q[18] = a > b;
  q[19] = a < b;
  q[20] = a >= b;
  q[21] = a <= b;
  q[22] = a && b;
  q[23] = a || b;
  q[24] = !a;
  q[25] = a & b;
  q[26] = a | b;
  q[27] = a ^ b;
  q[28] = ~a;
  q[29] -= a;
  q[30] *= b;
  q[31] /= b;
  q[32] %= a;
  q[33] <<= (125900uwb - b);
  q[34] >>= (125901uwb - b);
  q[35] &= a;
  q[36] |= b;
  q[37] ^= a;
  q[38] = sizeof (a);
  q[39] = q[39] ? a : b;
  q[40] = 24196214uwb;
  switch (a)
    {
    case 12345641uwb:
      if (b != 1244wb)
	__builtin_abort ();
      break;
    case 11821400uwb:
      if (b != 133445uwb)
	__builtin_abort ();
      break;
    case 12145uwb:
      if (b != 1212uwb)
	__builtin_abort ();
      break;
    case 24320389uwb:
      if (b != 125897uwb)
	__builtin_abort ();
      break;
    case 7128412uwb:
      if (b != 150uwb)
	__builtin_abort ();
      break;
    default:
      __builtin_abort ();
    }
}

int
main ()
{
  unsigned _BitInt(25) p;
  unsigned _BitInt(27) q[41];
  static unsigned _BitInt(27) qe[41] = {
    24320389uwb, 124983685uwb, 134216069uwb, 42uwb, 24320407uwb,
    24446286uwb, 24194492uwb, 89202797uwb, 193uwb, 22268uwb,
    2014352uwb, 1030781uwb, 8uwb, 10uwb, 10uwb,
    12uwb, 0uwb, 1uwb, 1uwb, 0uwb,
    1uwb, 0uwb, 1uwb, 1uwb, 0uwb,
    67969uwb, 24378317uwb, 24310348uwb, 9234042uwb, 109897346uwb,
    125897uwb, 0uwb, 52uwb, 40uwb, 771uwb,
    5uwb, 125917uwb, 24320384uwb, 1uwb, 24320389uwb,
    24196214uwb
  };
  q[4] = 18uwb;
  q[12] = 7uwb;
  q[13] = 9uwb;
  q[14] = 11uwb;
  q[15] = 13uwb;
  q[29] = 7uwb;
  q[30] = -1uwb;
  q[31] = -13uwb;
  q[32] = 52uwb;
  q[33] = 5uwb;
  q[34] = 12345uwb;
  q[35] = 15uwb;
  q[36] = 28uwb;
  q[37] = 5uwb;
  q[39] = 2uwb;
  foo (24320389uwb, 125897uwb, &p, q, 42.0f);
  if (p != 125897uwb)
    __builtin_abort ();
  q[38] -= sizeof (p) - 1;
  for (int i = 0; i < 41; ++i)
    if (q[i] != qe[i])
      __builtin_abort ();
  return 0;
}
