/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1" } */

static inline unsigned int
rshift_u_s (unsigned int left, int right)
{
  return left >> right;
}

unsigned int g_15;

int func_29 (int p_30)
{
  unsigned int l_31;
  unsigned long long int l_35 = 0x7736EAE11771B705LL;
  unsigned int l_36 = 0xEDB553A8L;

  l_31 = g_15;
  if ((l_31 <
       (rshift_u_s ((g_15 - (g_15 >= l_35)), (l_36 <= 1)))) + mod_rhs (1))
    return 1;
}

