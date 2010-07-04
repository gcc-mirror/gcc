typedef int int32_t;
typedef unsigned int uint32_t;
static uint32_t
safe_sub_func_uint32_t_u_u (uint32_t ui1, uint32_t ui2)
{
  return ui1 - ui2;
}

int32_t l_105[7];

int32_t g_4;
int32_t *g_54 = &g_4;
int32_t *
func (int32_t p_73, int32_t * p_74, int32_t p_75, int32_t * *p_76,
      int32_t * *p_77)
{
lbl_110:for (g_4 = 0; g_4; g_4 = 1)
    {
    }
  for (p_75 = -28; p_75; p_75 = safe_sub_func_uint32_t_u_u (p_75, 1))
    {
      if (g_4)
        goto lbl_110;
      *g_54 = 0;
    }
  return &l_105[5];
}

