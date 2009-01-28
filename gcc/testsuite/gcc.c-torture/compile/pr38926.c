static inline int foo (unsigned _si1)
{
  if (_si1 != 0)
    if (_si1 > 2147483647)
      return 1;
  return 0;
}

static inline unsigned bar (unsigned _left, int _right)
{
  return (unsigned) _right >= 8 ? 1 : _left >> _right;
}

unsigned g_2;
unsigned g_67;
volatile unsigned g_162;

static inline int func_62 (unsigned p_63)
{
  p_63 = g_2 & g_67;
  if (g_2)
    ;
  else if (p_63)
    return 1;
  g_67 = bar (p_63, g_2);
  return 0;
}

unsigned baz (void)
{
  if (g_2)
    for (; g_2 <= -16; g_2 = foo (g_2))
      {
        for (; g_162; g_162)
          func_62 (func_62 (0));
        if (g_67)
          break;
      }
  return g_2;
}

