int v = 0;

int
atomic_fetch_add_SEQ_CST (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_sub_SEQ_CST (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_and_SEQ_CST (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_nand_SEQ_CST (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_xor_SEQ_CST (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_or_SEQ_CST (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_SEQ_CST);
}
