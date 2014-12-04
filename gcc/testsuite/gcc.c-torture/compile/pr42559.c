/* { dg-require-effective-target label_values } */

void jumpfunc(int copy, void *p)
{
  void *l = &&jumplabel;
  if (copy)
    __builtin___memcpy_chk (p, l, 128, __builtin_object_size (p, 0));
jumplabel:
  return;
}
