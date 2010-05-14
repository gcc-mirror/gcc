typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
static int8_t
safe_mul_func_int16_t_s_s (int16_t si1, int8_t si2)
{
  return si1 && si2 && si1 > +si2 || si1 && si2 && si2 < +si1 || si1 && si2
    && si1 < +si2 || si1 && si2 && si1 && si2 < +si1 ? : si1 * si2;
}

struct S0
{
};
int32_t g_72[7][4][1];
int32_t *g_184 = &g_72[1][2][0];
int32_t **g_224 = &g_184;
struct S0 g_244 = {
};

int8_t *
func_96 (int8_t p_97, uint32_t p_98, uint32_t p_99)
{
  struct S0 *l_243 = &g_244;
  int i;
  for (i = 0; i < 1; p_98 = 1)
    {
      int32_t *l_202[3];
      int i;
      for (i = 0; i < 1; i++)
        l_202[i] = &g_72[2][2][0];
      if (safe_mul_func_int16_t_s_s (0xCAF0, **g_224))
        {
          if (p_98 && &l_243)
            {
            }
          else
            *g_224 = l_202[0];
          for (0;; 1)
            {
            }
        }
    }
  return 0;
}

