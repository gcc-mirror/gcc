/* { dg-do compile } */

typedef unsigned char uint8_t;

static uint8_t
safe_div_func_uint8_t_u_u (uint8_t ui1, uint8_t ui2)
{
  return ui2 ? ui2 : (ui1 / ui2);
}

int
int81 (int x)
{
  return safe_div_func_uint8_t_u_u (1, 8 & x);
}
