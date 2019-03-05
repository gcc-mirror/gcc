/* PR tree-optimization/88659 - ICE in maybe_warn_nonstring_arg
   { dg-do compile }
   { dg-options "-O0 -Wall" }  */

const char a[5] = "1234";

int cst_idx_cst_bnd (void)
{
  return __builtin_strnlen (&a[1], 0);
}

int var_idx_cst_bnd (void)
{
  int i = 1;
  return __builtin_strnlen (&a[i], 0);
}

int phi_idx_cst_bnd (int i)
{
  return __builtin_strnlen (&a[i ? 1 : 2], 0);
}

int unk_idx_cst_bnd (int i)
{
  return __builtin_strnlen (&a[i], 0);
}

int cst_idx_var_bnd (void)
{
  int n = 0;
  return __builtin_strnlen (&a[1], n);
}

int cst_idx_phi_bnd (int n)
{
  return __builtin_strnlen (&a[1], n ? 1 : 2);
}

int cst_idx_unk_bnd (int n)
{
  return __builtin_strnlen (&a[1], n);
}
