int v;

void
sync_lock_release (void)
{
  __sync_lock_release (&v);
}
