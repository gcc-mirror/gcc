/* PR rtl-optimization/44031 */
/* Testcase by John Regehr <regehr@cs.utah.edu> */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;

static uint32_t
safe_add_func_uint32_t_u_u (uint32_t ui1, uint32_t ui2)
{
  return ui1 + ui2;
}

static uint64_t
safe_div_func_uint64_t_u_u (uint64_t ui1, uint32_t ui2)
{
  return ui2 ? : (ui1 / ui2);
}

uint8_t g_55;
uint8_t *g_73 = &g_55;
uint8_t **g_332 = &g_73;

int func_38(uint8_t *,int);
int func_8(int);

int int321 (void)
{
  uint8_t l_26[4];
  uint8_t *l_238 = &l_26[2];
  uint8_t l_400;
  l_400 &=
    func_38 (&l_26[3],
         safe_add_func_uint32_t_u_u (safe_div_func_uint64_t_u_u
                     (1, **g_332),
                     *l_238) >= *l_238 < func_8 (0)), 1;
  return 0;
}
