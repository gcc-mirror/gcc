static void
do_something (int flag)
{
  if (flag)
    throw 42; // { dg-warning "leak of 'ptr'" }
}

int test (int flag)
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }

  do_something (flag);

  __builtin_free  (ptr);
  return 0;
}
