/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

__attribute__((noipa)) void
foo (_BitInt(6) a, _BitInt(27) b, _BitInt(6) *p, _BitInt(27) *q, float c)
{
  p[0] = b;
  q[0] = a;
  q[1] = (unsigned _BitInt(6)) a;
  q[2] = (unsigned _BitInt(9)) a;
  q[3] = c;
  q[4] += a;
  q[5] = a + b;
  q[6] = a - b;
  q[7] = a * b;
  q[8] = a / b;
  q[9] = a % b;
  q[10] = b << (-20wb - a);
  q[11] = (b * 131wb) >> (-20wb - a);
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
  q[33] <<= b;
  q[34] >>= b;
  q[35] &= a;
  q[36] |= b;
  q[37] ^= a;
  q[38] = sizeof (a);
  q[39] = q[39] ? a : b;
  q[40] = 12345wb;
  switch (a)
    {
    case 31wb:
      if (b != 8wb)
	__builtin_abort ();
      break;
    case -18wb:
      if (b != 9wb)
	__builtin_abort ();
      break;
    case 26wb:
      if (b != 12wb)
	__builtin_abort ();
      break;
    case -25wb:
      if (b != 6wb)
	__builtin_abort ();
      break;
    case -19wb:
      if (b != 15wb)
	__builtin_abort ();
      break;
    default:
      __builtin_abort ();
    }
}

int
main ()
{
  _BitInt(6) p;
  _BitInt(27) q[41];
  static _BitInt(27) qe[41] = {
    -25wb, 39wb, 487wb, 17wb, -7wb, -19wb, -31wb, -150wb, -4wb, -1,
    192wb, 24wb, 8wb, 10wb, 10wb, 12wb, 0, 1wb, 0wb, 1,
    0, 1, 1, 1wbu, 0, 6wb, -25wb, -31wb, 24wb, 32wb,
    -6wb, -2wb, 2uwb, 320wb, 192wb, 7wb, 30wb, -30wb, 1, -25wb,
    12345wb
  };
  q[4] = 18wb;
  q[12] = 7wb;
  q[13] = 9wb;
  q[14] = 11wb;
  q[15] = 13wb;
  q[29] = 7wb;
  q[30] = -1wb;
  q[31] = -13wb;
  q[32] = 52wb;
  q[33] = 5wb;
  q[34] = 12345wb;
  q[35] = 15wb;
  q[36] = 28wb;
  q[37] = 5wb;
  q[39] = 2wb;
  foo (-25wb, 6wb, &p, q, 17.0f);
  if (p != 6wb)
    __builtin_abort ();
  q[38] -= sizeof (p) - 1;
  for (int i = 0; i < 41; ++i)
    if (q[i] != qe[i])
      __builtin_abort ();
  return 0;
}
