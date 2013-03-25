int v = 0;

int
atomic_fetch_add_RELAXED (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELAXED);
}

int
atomic_fetch_sub_RELAXED (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELAXED);
}

int
atomic_fetch_and_RELAXED (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELAXED);
}

int
atomic_fetch_nand_RELAXED (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELAXED);
}

int
atomic_fetch_xor_RELAXED (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELAXED);
}

int
atomic_fetch_or_RELAXED (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELAXED);
}
