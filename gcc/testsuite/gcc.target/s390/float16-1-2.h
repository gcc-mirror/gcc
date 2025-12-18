unsigned short
fpr_to_gpr (_Float16 x)
{
  unsigned short y;
  __builtin_memcpy (&y, &x, 2);
  return y;
}

_Float16
gpr_to_fpr (unsigned short x)
{
  _Float16 y;
  __builtin_memcpy (&y, &x, 2);
  return y;
}

_Float16
load_into_fpr (_Float16 *x)
{
  return *x;
}

unsigned short
load_into_gpr (_Float16 *x)
{
  _Float16 xx = *x;
  unsigned short y;
  __builtin_memcpy (&y, &xx, 2);
  return y;
}

void
store (_Float16 *x, _Float16 y)
{
  *x = y;
}
