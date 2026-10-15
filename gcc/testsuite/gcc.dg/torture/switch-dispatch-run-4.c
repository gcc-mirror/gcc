/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch" } */

/* Keep floating values live across the dispatches.  Targets with stack
   registers must retain a single table owner for stack conversion.  */

__attribute__((noipa)) long double
dispatch (const unsigned char *pc, long double seed, volatile unsigned *out)
{
  long double a = seed + 1, b = seed + 2, c = seed + 3, d = seed + 4;

  for (;;)
    switch (*pc++)
      {
      case 0: return a + 3 * b + 5 * c + 7 * d;
      case 1: a += b; out[0] += 1; break;
      case 2: b += c; out[1] += 2; break;
      case 3: c += d; out[2] += 3; break;
      case 4: d += a; out[3] += 4; break;
      case 5: a -= c; out[0] += 5; break;
      case 6: b -= d; out[1] += 6; break;
      case 7: c -= a; out[2] += 7; break;
      case 8: d -= b; out[3] += 8; break;
      case 9: a *= 2; out[0] += 9; break;
      case 10: b *= 2; out[1] += 10; break;
      case 11: c *= 2; out[2] += 11; break;
      default: d *= 2; out[3] += 12; break;
      }
}

int
main (void)
{
  static const unsigned char code[] = {
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 255, 1, 2, 3, 0
  };
  for (unsigned seed = 0; seed != 16; ++seed)
    {
      volatile unsigned out[4] = { 0 };
      long double a = seed + 1, b = seed + 2, c = seed + 3, d = seed + 4;
      a += b;
      b += c;
      c += d;
      d += a;
      a -= c;
      b -= d;
      c -= a;
      d -= b;
      a *= 2;
      b *= 2;
      c *= 2;
      d *= 2;
      a += b;
      b += c;
      c += d;
      if (dispatch (code, seed, out) != a + 3 * b + 5 * c + 7 * d
          || out[0] != 16 || out[1] != 20 || out[2] != 24 || out[3] != 24)
        __builtin_abort ();
    }
  return 0;
}
