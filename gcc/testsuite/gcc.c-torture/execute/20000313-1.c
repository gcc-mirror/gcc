unsigned int buggy (unsigned int *param)
{
  unsigned int accu, zero = 0, borrow;
  accu    = - *param;
  borrow  = - (accu > zero);
  *param += accu;
  return borrow;
}

int main (void)
{
  unsigned int param  = 1;
  unsigned int borrow = buggy (&param);

  if (param != 0)
    abort ();
  if (borrow + 1 != 0)
    abort ();
  return 0;
}
