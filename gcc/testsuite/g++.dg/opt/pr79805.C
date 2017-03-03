// PR middle-end/79805
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions" }

struct A { A (); ~A (); };

void bar (void);

int
f0 (int *d, int f)
{
  A z;
  int e = __atomic_compare_exchange_n (d, &f, 1, 1, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  return e;
}

int
f1 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

int
f2 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  unsigned int t1 = __atomic_fetch_or (a, mask, __ATOMIC_RELAXED);
  unsigned int t2 = t1 & mask;
  return t2 != 0;
}

long int
f3 (long int *a, int bit)
{
  A z;
  unsigned long int mask = (1ul << bit);
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) == 0;
}

int
f4 (int *a)
{
  A z;
  unsigned int mask = (1u << 7);
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

int
f5 (int *a)
{
  A z;
  unsigned int mask = (1u << 13);
  return (__atomic_fetch_or (a, mask, __ATOMIC_RELAXED) & mask) != 0;
}

int
f6 (int *a)
{
  A z;
  unsigned int mask = (1u << 0);
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

void
f7 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  if ((__sync_fetch_and_xor (a, mask) & mask) != 0)
    bar ();
}

void
f8 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  if ((__atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask) == 0)
    bar ();
}

int
f9 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

int
f10 (int *a)
{
  A z;
  unsigned int mask = (1u << 7);
  return (__sync_fetch_and_xor (a, mask) & mask) != 0;
}

int
f11 (int *a)
{
  A z;
  unsigned int mask = (1u << 13);
  return (__atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask) != 0;
}

int
f12 (int *a)
{
  A z;
  unsigned int mask = (1u << 0);
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

int
f13 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__sync_fetch_and_and (a, ~mask) & mask) != 0;
}

int
f14 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask) != 0;
}

int
f15 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

int
f16 (int *a)
{
  A z;
  unsigned int mask = (1u << 7);
  return (__sync_fetch_and_and (a, ~mask) & mask) != 0;
}

int
f17 (int *a)
{
  A z;
  unsigned int mask = (1u << 13);
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask) != 0;
}

int
f18 (int *a)
{
  A z;
  unsigned int mask = (1u << 0);
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

unsigned long int
f19 (unsigned long int *a, int bit)
{
  A z;
  unsigned long int mask = (1ul << bit);
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

unsigned long int
f20 (unsigned long int *a)
{
  A z;
  unsigned long int mask = (1ul << 7);
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask) == 0;
}

int
f21 (int *a, int bit)
{
  A z;
  unsigned int mask = (1u << bit);
  return (__sync_fetch_and_or (a, mask) & mask);
}

unsigned long int
f22 (unsigned long int *a)
{
  A z;
  unsigned long int mask = (1ul << 7);
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask);
}

unsigned long int
f23 (unsigned long int *a)
{
  A z;
  unsigned long int mask = (1ul << 7);
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask);
}

unsigned short int
f24 (unsigned short int *a)
{
  A z;
  unsigned short int mask = (1u << 7);
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

unsigned short int
f25 (unsigned short int *a)
{
  A z;
  unsigned short int mask = (1u << 7);
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}
