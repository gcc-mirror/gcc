int v = 0;

int
atomic_fetch_add_RELEASE (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_sub_RELEASE (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_and_RELEASE (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_nand_RELEASE (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_xor_RELEASE (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_or_RELEASE (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELEASE);
}
