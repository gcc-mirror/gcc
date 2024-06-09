/* { dg-do compile } */
/* { dg-options "-finline-stringops -fno-lto" } */

void *zero (unsigned long long (*p)[32], int n)
{
  return __builtin_memset (p, 0, n * sizeof (*p));
}

void *ones (char (*p)[128], int n)
{
  return __builtin_memset (p, -1, n * sizeof (*p));
}

void *opt2 (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 1024 : 2) * sizeof (*p));
}

void *opt8 (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 1024 : 8) * sizeof (*p));
}

void *opt32 (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 1024 : 32) * sizeof (*p));
}

void *opt128 (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 1024 : 128) * sizeof (*p));
}

void *opt512 (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 1024 : 512) * sizeof (*p));
}

void *opt_primes (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 509 : 7) * sizeof (*p));
}

void *opt_primes_blk (int *p, int i)
{
  return __builtin_memset (p, 0, (i ? 521 : 9) * sizeof (*p));
}

void *huge (long (*p)[16384])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep1 (long (*p)[16384+1])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep4 (long (*p)[16384+4])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep16 (long (*p)[16384+16])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep64 (long (*p)[16384+64])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep256 (long (*p)[16384+256])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

void *hugep1024p256p64p16p4p1 (long (*p)[16384+1024+64+16+4+1])
{
  return __builtin_memset (p, 0, sizeof (*p));
}

/* { dg-final { scan-assembler-not {\mmemset\M} } } */
