/* { dg-do run } */

typedef __UINT16_TYPE__ uint16_t;
typedef __INT32_TYPE__ int32_t;
static uint16_t g_2823 = 0xEC75L;
static uint16_t g_116 = 0xBC07L;

static uint16_t
safe_mul_func_uint16_t_u_u(uint16_t ui1, uint16_t ui2)
{
  return ((unsigned int)ui1) * ((unsigned int)ui2);
}

int main ()
{
  uint16_t l_2815 = 0xffff;
  uint16_t *l_2821 = &g_116;
  uint16_t *l_2822 = &g_2823;

lbl_2826:
  l_2815 &= 0x1eae;
  if (safe_mul_func_uint16_t_u_u(((*l_2821) = l_2815), (--(*l_2822))))
    goto lbl_2826;
  if (g_2823 != 32768)
    __builtin_abort ();
  return 0;
}
