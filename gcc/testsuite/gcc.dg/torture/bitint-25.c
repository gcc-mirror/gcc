/* PR c/102989 */
/* { dg-do run { target { bitint && float16_runtime } } } */
/* { dg-options "-std=gnu23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */
/* { dg-add-options float16 } */

int
main ()
{
#if __FLT16_MANT_DIG__ == 11
  static volatile _Float16 s[] = {
    0.0F16,
    42.0F16,
    -65504.0F16,
    65504.0F16
  };
  static volatile _Float16 u[] = {
    0.0F16,
    1.0F16,
    178.0F16,
    65504.0F16
  };
#if __BITINT_MAXWIDTH__ >= 135
  static volatile _BitInt(135) s_135[] = {
    0wb,
    42wb,
    -65504wb,
    65504wb
  };
  static volatile unsigned _BitInt(135) u_135[] = {
    0uwb,
    1uwb,
    178uwb,
    65504uwb
  };
  for (int i = 0; i < 4; ++i)
    {
      if (s[i] != (_Float16) s_135[i]
	  || u[i] != (_Float16) u_135[i]
	  || (_BitInt(135)) s[i] != s_135[i]
	  || (unsigned _BitInt(135)) u[i] != u_135[i])
	__builtin_abort ();
    }
#endif
#if __BITINT_MAXWIDTH__ >= 192
  static volatile _BitInt(192) s_192[] = {
    0wb,
    42wb,
    -65504wb,
    65504wb
  };
  static volatile unsigned _BitInt(192) u_192[] = {
    0uwb,
    1uwb,
    178uwb,
    65504uwb
  };
  for (int i = 0; i < 4; ++i)
    {
      if (s[i] != (_Float16) s_192[i]
	  || u[i] != (_Float16) u_192[i]
	  || (_BitInt(192)) s[i] != s_192[i]
	  || (unsigned _BitInt(192)) u[i] != u_192[i])
	__builtin_abort ();
    }
#endif
#if __BITINT_MAXWIDTH__ >= 575
  static volatile _BitInt(575) s_575[] = {
    0wb,
    42wb,
    -65504wb,
    65504wb
  };
  static volatile unsigned _BitInt(575) u_575[] = {
    0uwb,
    1uwb,
    178uwb,
    65504uwb
  };
  for (int i = 0; i < 4; ++i)
    {
      if (s[i] != (_Float16) s_575[i]
	  || u[i] != (_Float16) u_575[i]
	  || (_BitInt(575)) s[i] != s_575[i]
	  || (unsigned _BitInt(575)) u[i] != u_575[i])
	__builtin_abort ();
    }
#endif
#endif
}
