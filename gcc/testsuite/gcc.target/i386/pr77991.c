/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=short" } */

struct rcu_reader_data
{
  unsigned ctr;
  _Bool waiting;
}

extern __thread rcu_reader;

void rcu_read_lock()
{
  struct rcu_reader_data *x = &rcu_reader;
  _Bool val = 0;

  __atomic_store(&x->waiting, &val, 0);
}
