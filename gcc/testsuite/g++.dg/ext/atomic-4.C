// PR inline-asm/85172
// { dg-do compile }
// { dg-options "" }

int
foo (int *p)
{
  return !__atomic_always_lock_free (4, ({ __asm (""); p; }));
}
