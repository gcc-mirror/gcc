short v = 0;

short
atomic_fetch_add_RELAXED (short a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELAXED);
}

short
atomic_fetch_sub_RELAXED (short a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELAXED);
}

short
atomic_fetch_and_RELAXED (short a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELAXED);
}

short
atomic_fetch_nand_RELAXED (short a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELAXED);
}

short
atomic_fetch_xor_RELAXED (short a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELAXED);
}

short
atomic_fetch_or_RELAXED (short a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELAXED);
}
