char v = 0;

char
atomic_fetch_add_RELAXED (char a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_sub_RELAXED (char a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_and_RELAXED (char a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_nand_RELAXED (char a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_xor_RELAXED (char a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_or_RELAXED (char a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELAXED);
}
