typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
static inline uint32_t
safe_add_int8_t_s_s (int8_t si1, int16_t si2)
{
  if ((si1) && (si2) && (si1 > (1 - si2)) || (si1) && (si2 < 0)
      && (si1 < (-128 - si2)))
    return si1;
  return si1 + si2;
}

uint32_t g_2;
uint32_t g_113;
uint32_t g_145;
int32_t
func_17 (int32_t p_18, uint32_t p_19, uint32_t p_21)
{
  uint32_t l_23 = -1L;
  return l_23;
}

uint32_t
func_26 (uint16_t p_27)
{
  uint32_t l_424;
  if (func_93 (func_59 (safe_add_int8_t_s_s (p_27, 1))),
      func_124 (l_424, -7L, 1, g_145, 1, (safe_add_int8_t_s_s (1, 1)), 1), 1,
      1, 1)
    func_117 (1, 1,
              (safe_add_uint64_t_u_u
               (1, (safe_add_int8_t_s_s (1, 0xCDF4BE7A1B7E4629LL)))), 1);
  uint32_t l_210;
  if (func_17
      ((safe_add_int8_t_s_s (g_2, (0x6C79A83AL | func_17 (1, 1, 1)))),
       0x4C9FL, 1))
    {
      uint32_t l_212;
      if (safe_mul_int32_t_s_s
          ((1, 1, l_212, (1, (safe_add_int8_t_s_s (l_210, 1)), 1, 1)), 1))
        if (func_59 (1, (safe_add_int8_t_s_s (g_113, 1))))
          {
          }
    }
}

