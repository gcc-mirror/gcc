/* { dg-do compile } */

typedef char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;

static int16_t
safe_rshift_func_int16_t_s_u (int16_t left, unsigned int right)
{
  return left || right >= 1 * 8 ? left : left >> right;
}

static int8_t
safe_rshift_func_int8_t_s_u (int8_t left, unsigned int right)
{
  return left || right >= 1 * 8 ? left : left >> right;
}


static uint32_t
safe_add_func_uint32_t_u_u (uint32_t ui1, uint16_t ui2)
{
  return ui1 + ui2;
}

int16_t g_4;
int8_t g_4_8;
uint32_t g_9[1];
uint32_t g_9_8[2];
int161 (void)
{
  int32_t l_2;

  for (l_2 = -25; l_2; l_2 = safe_add_func_uint32_t_u_u (l_2, 1))
    g_9[0] ^= safe_rshift_func_int16_t_s_u (g_4, 1);
}

int81 (void)
{
  int32_t l_2;

  for (l_2 = -25; l_2; l_2 = safe_add_func_uint32_t_u_u (l_2, 1))
    {
      g_9[0] ^= safe_rshift_func_int8_t_s_u (g_4_8, 1);
      g_9[1] ^= safe_rshift_func_int8_t_s_u (g_4_8, 1);
    }

  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

