int v = 0;

int
sync_fetch_and_add (int a)
{
  return __sync_fetch_and_add (&v, a);
}

int
sync_fetch_and_sub (int a)
{
  return __sync_fetch_and_sub (&v, a);
}

int
sync_fetch_and_and (int a)
{
  return __sync_fetch_and_and (&v, a);
}

int
sync_fetch_and_nand (int a)
{
  return __sync_fetch_and_nand (&v, a);
}

int
sync_fetch_and_xor (int a)
{
  return __sync_fetch_and_xor (&v, a);
}

int
sync_fetch_and_or (int a)
{
  return __sync_fetch_and_or (&v, a);
}

int
sync_add_and_fetch (int a)
{
  return __sync_add_and_fetch (&v, a);
}

int
sync_sub_and_fetch (int a)
{
  return __sync_sub_and_fetch (&v, a);
}

int
sync_and_and_fetch (int a)
{
  return __sync_and_and_fetch (&v, a);
}

int
sync_nand_and_fetch (int a)
{
  return __sync_nand_and_fetch (&v, a);
}

int
sync_xor_and_fetch (int a)
{
  return __sync_xor_and_fetch (&v, a);
}

int
sync_or_and_fetch (int a)
{
  return __sync_or_and_fetch (&v, a);
}
