int v;

int
sync_lock_test_and_set (int a)
{
  return __sync_lock_test_and_set (&v, a);
}
