struct ieee754_double {
  double d;
};
extern const float __exp_deltatable[178];
float __ieee754_expf (float x)
{
  static const float himark = 88.72283935546875;
  static const float lomark = -103.972084045410;
  if (__builtin_isless(x, himark) && __builtin_isgreater(x, lomark))
    {
      int tval;
      double x22, t, result, dx;
      float delta;
      struct ieee754_double ex2_u;
      dx -= t;
      tval = (int) (t * 512.0);
      if (t >= 0)
	delta = - __exp_deltatable[tval];
      else
	delta = __exp_deltatable[-tval];
      x22 = (0.5000000496709180453 * dx + 1.0000001192102037084) * dx + delta;
      result = x22 * ex2_u.d + ex2_u.d;
      return (float) result;
    }
  return x;
}
