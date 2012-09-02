typedef int int32_t;
typedef unsigned char uint8_t;
struct S0
{
  uint8_t f0;
};
struct S0 *g_18[7][5][1][1] = {
};

struct S0 **g_17 = &g_18[0][3][0][0];
int32_t g_86;
struct S0 func_72 (uint8_t p_73, struct S0 p_74);

void
int326 (struct S0 **p_67, int32_t p_68, int32_t * *const p_69,
	struct S0 *p_70)
{
  struct S0 l_95 = {
    -1L
  };
  func_72 (1L, func_72 (0, l_95));
}

struct S0
func_72 (uint8_t p_73, struct S0 p_74)
{
  int32_t *l_85 = &g_86;
  if (*l_85)
  lbl_94:*l_85 ^= 0;
  if (g_86)
    goto lbl_94;
  return **g_17;
}
