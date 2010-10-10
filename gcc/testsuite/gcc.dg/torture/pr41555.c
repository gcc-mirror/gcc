/* { dg-do run } */
/* { dg-options "-std=c99" } */
/* { dg-require-effective-target stdint_types } */

#include <stdint.h>
#include <limits.h>

extern void abort (void);

static uint64_t safe_div_func_uint64_t_u_u (uint64_t _ui1, uint64_t _ui2)
{
  if (_ui2==0) return _ui1;
  return _ui1 / _ui2;
}

static int64_t safe_div_func_int64_t_s_s (int64_t _si1, int64_t _si2)
{
  if (_si2==0 || (_si1==INT64_MIN && _si2==-1)) return _si1;
  return _si1 / _si2;
}

#define safe_add_macro_int8_t_s_s(si1,si2) \
                ((((((int8_t)(si1))>((int8_t)0)) && (((int8_t)(si2))>((int8_t)0)) && (((int8_t)(si1)) > ((INT8_MAX)-((int8_t)(si2))))) \
                  || ((((int8_t)(si1))<((int8_t)0)) && (((int8_t)(si2))<((int8_t)0)) && (((int8_t)(si1)) < ((INT8_MIN)-((int8_t)(si2)))))) \
                 ? ((int8_t)(si1)) \
                 : (((int8_t)(si1)) + ((int8_t)(si2))) \
                 ) 

static int8_t
safe_add_func_int8_t_s_s(int8_t _si1, int8_t _si2)
{
  return safe_add_macro_int8_t_s_s(_si1,_si2);
}

#define safe_rshift_macro_uint64_t_u_s(left,right) \
        (((((int)(right)) < ((uint64_t)0)) \
                         || (((int)(right)) >= sizeof(uint64_t)*CHAR_BIT)) \
                        ? ((uint64_t)(left)) \
                        : (((uint64_t)(left)) >> ((int)(right))))

static uint64_t
safe_rshift_func_uint64_t_u_s(uint64_t _left, int _right)
{
  return safe_rshift_macro_uint64_t_u_s(_left,_right);
}

#define safe_mul_macro_int32_t_s_s(si1,si2) \
  ((((((int32_t)(si1)) > ((int32_t)0)) && (((int32_t)(si2)) > ((int32_t)0)) && (((int32_t)(si1)) > ((INT32_MAX) / ((int32_t)(si2))))) || \
  ((((int32_t)(si1)) > ((int32_t)0)) && (((int32_t)(si2)) <= ((int32_t)0)) && (((int32_t)(si2)) < ((INT32_MIN) / ((int32_t)(si1))))) || \
  ((((int32_t)(si1)) <= ((int32_t)0)) && (((int32_t)(si2)) > ((int32_t)0)) && (((int32_t)(si1)) < ((INT32_MIN) / ((int32_t)(si2))))) || \
  ((((int32_t)(si1)) <= ((int32_t)0)) && (((int32_t)(si2)) <= ((int32_t)0)) && (((int32_t)(si1)) != ((int32_t)0)) && (((int32_t)(si2)) < ((INT32_MAX) / ((int32_t)(si1)))))) \
  ? ((int32_t)(si1)) \
  : ((int32_t)(si1)) * ((int32_t)(si2)))

static int32_t
safe_mul_func_int32_t_s_s (int32_t _si1, int32_t _si2)
{
  return safe_mul_macro_int32_t_s_s(_si1,_si2);
}

static int8_t g_39;
static volatile uint8_t g_46;
static uint8_t g_47;
static uint8_t *g_62;
static uint8_t g_79;
static int8_t g_101 = -1L;
static uint8_t *g_114;
static uint8_t *g_126;
static uint8_t g_133;

static uint16_t func_35 (int32_t * p_36, uint64_t p_37, uint32_t p_38);
static uint16_t func_35 (int32_t * p_36, uint64_t p_37, uint32_t p_38)
{
  if (g_62 != 0)
    abort ();
  for (g_39 = 1; g_39 < 0; g_39 = 1)
    {
    }
  return 1;
}

static int32_t func_19 (int32_t p_20);
static int32_t func_19 (int32_t p_20)
{
  if (1 !=
      safe_div_func_uint64_t_u_u ((safe_div_func_int64_t_s_s (p_20, 1)),
                                  g_101))
    {
      func_35 (0, 1 <= (safe_add_func_int8_t_s_s (g_47, g_46)) > p_20 < 1, 1);
      g_133 = 1;
      if (g_114 != 0)
	abort ();
      if (g_126 != 0)
	abort ();
    }
  return 1;
}

static uint8_t func_2 (int32_t p_6);
static uint8_t func_2 (int32_t p_6)
{
  for (1; p_6 > 1; 1)
    return 0;
  func_19 (g_79);
  if (safe_mul_func_int32_t_s_s
      ((0, 1 < (safe_rshift_func_uint64_t_u_s (1 ^ p_6, 1))),
       (func_35 (&p_6, 1, 1) < 1)))
    {
    }
  return 1;
}

int main (void)
{
  func_2 (1);
  if (g_133 != 1)
    abort ();
  return 0;
}

