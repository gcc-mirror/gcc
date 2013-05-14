int v = 0;

int
atomic_fetch_add_ACQUIRE (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_sub_ACQUIRE (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_and_ACQUIRE (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_nand_ACQUIRE (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_xor_ACQUIRE (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_or_ACQUIRE (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_ACQUIRE);
}
