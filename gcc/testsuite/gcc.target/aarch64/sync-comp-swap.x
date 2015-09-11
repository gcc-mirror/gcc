int v = 0;

int
sync_bool_compare_swap (int a, int b)
{
  return __sync_bool_compare_and_swap (&v, &a, &b);
}

int
sync_val_compare_swap (int a, int b)
{
  return __sync_val_compare_and_swap (&v, &a, &b);
}
