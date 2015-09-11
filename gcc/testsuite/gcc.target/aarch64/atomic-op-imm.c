/* { dg-do compile } */
/* { dg-options "-march=armv8-a+nolse -O2" } */

int v = 0;

int
atomic_fetch_add_RELAXED ()
{
  return __atomic_fetch_add (&v, 4096, __ATOMIC_RELAXED);
}

int
atomic_fetch_sub_ACQUIRE ()
{
  return __atomic_fetch_sub (&v, 4096, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_add_negative_RELAXED ()
{
  return __atomic_fetch_add (&v, -4096, __ATOMIC_RELAXED);
}

int
atomic_fetch_sub_negative_ACQUIRE ()
{
  return __atomic_fetch_sub (&v, -4096, __ATOMIC_ACQUIRE);
}

int
atomic_fetch_and_SEQ_CST ()
{
  return __atomic_fetch_and (&v, 4096, __ATOMIC_SEQ_CST);
}

int
atomic_fetch_nand_ACQ_REL ()
{
  return __atomic_fetch_nand (&v, 4096, __ATOMIC_ACQ_REL);
}

int
atomic_fetch_xor_CONSUME ()
{
  return __atomic_fetch_xor (&v, 4096, __ATOMIC_CONSUME);
}

int
atomic_fetch_or_RELAXED ()
{
  return __atomic_fetch_or (&v, 4096, __ATOMIC_RELAXED);
}

int
atomic_add_fetch_ACQUIRE ()
{
  return __atomic_add_fetch (&v, 4096, __ATOMIC_ACQUIRE);
}

int
atomic_sub_fetch_RELAXED ()
{
  return __atomic_sub_fetch (&v, 4096, __ATOMIC_RELAXED);
}

int
atomic_and_fetch_SEQ_CST ()
{
  return __atomic_and_fetch (&v, 4096, __ATOMIC_SEQ_CST);
}

int
atomic_nand_fetch_ACQUIRE ()
{
  return __atomic_nand_fetch (&v, 4096, __ATOMIC_ACQUIRE);
}

int
atomic_xor_fetch_RELEASE ()
{
  return __atomic_xor_fetch (&v, 4096, __ATOMIC_RELEASE);
}

int
atomic_or_fetch_CONSUME ()
{
  return __atomic_or_fetch (&v, 4096, __ATOMIC_CONSUME);
}

/* { dg-final { scan-assembler-times "\tw\[0-9\]+, w\[0-9\]+, #*4096" 14 } } */
