/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */

float
foo (float a, float b, float c, float d)
{
  float ac, bd, ad, bc, y;

  ac = a * c;
  bd = b * d;
  ad = a * d;
  bc = b * c;

  if (__builtin_expect (!__builtin_expect ((a) != (a), 0)
			& !__builtin_expect (!__builtin_expect (((a) - (a)) != ((a) - (a)), 0), 1), 0)
      || __builtin_expect (!__builtin_expect ((b) != (b), 0)
			   & !__builtin_expect (!__builtin_expect (((b) - (b)) != ((b) - (b)), 0), 1), 0))
    a = __builtin_copysignf (__builtin_expect (!__builtin_expect ((a) != (a), 0)
					       & !__builtin_expect (!__builtin_expect (((a) - (a)) != ((a) - (a)), 0), 1), 0) ? 1 : 0, a);

  c = __builtin_copysignf (__builtin_expect (!__builtin_expect ((c) != (c), 0) & !__builtin_expect (!__builtin_expect (((c) - (c)) != ((c) - (c)), 0), 1), 0) ? 1 : 0, c);
  if ((__builtin_expect (!__builtin_expect ((ac) != (ac), 0)
			 & !__builtin_expect (!__builtin_expect (((ac) - (ac)) != ((ac) - (ac)), 0), 1), 0)
       || __builtin_expect (!__builtin_expect ((bd) != (bd), 0)
			    & !__builtin_expect (!__builtin_expect (((bd) - (bd)) != ((bd) - (bd)), 0), 1), 0)
       || __builtin_expect (!__builtin_expect ((bc) != (bc), 0) & !__builtin_expect (!__builtin_expect (((bc) - (bc)) != ((bc) - (bc)), 0), 1), 0)))
    d = __builtin_copysignf (0, d);

  y = a * d + b * c;

  return y;
}
