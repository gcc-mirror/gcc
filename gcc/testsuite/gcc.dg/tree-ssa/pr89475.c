/* PR tree-optimization/89475 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "link_error " "optimized" } } */

void link_error (void);

unsigned short
f0 (unsigned short x)
{
  x &= 0xaa55;
  x = __builtin_bswap16 (x);
  if (x & 0xaa55)
    link_error ();
  return x;
}

unsigned short
f1 (unsigned short x)
{
  x &= 0x55aa;
  x = __builtin_bswap16 (x);
  if (x & 0x55aa)
    link_error ();
  return x;
}

unsigned int
f2 (unsigned int x)
{
  x &= 0x55aa5aa5U;
  x = __builtin_bswap32 (x);
  if (x & 0x5aa555aaU)
    link_error ();
  return x;
}

unsigned long long int
f3 (unsigned long long int x)
{
  x &= 0x55aa5aa544cc2211ULL;
  x = __builtin_bswap64 (x);
  if (x & 0xeedd33bb5aa555aaULL)
    link_error ();
  return x;
}

unsigned short
f4 (unsigned short x)
{
  x = __builtin_bswap32 (x);
  if (x != 0)
    link_error ();
  return x;
}

unsigned int
f5 (unsigned int x)
{
  x = __builtin_bswap64 (x);
  if (x != 0)
    link_error ();
  return x;
}

unsigned short
f6 (unsigned short x)
{
  x |= 0xaa55;
  x = __builtin_bswap16 (x);
  if ((x | 0xaa55) != 0xffff)
    link_error ();
  return x;
}

unsigned short
f7 (unsigned short x)
{
  x |= 0x55aa;
  x = __builtin_bswap16 (x);
  if ((x | 0x55aa) != 0xffff)
    link_error ();
  return x;
}

unsigned int
f8 (unsigned int x)
{
  x |= 0x55aa5aa5U;
  x = __builtin_bswap32 (x);
  if ((x | 0x5aa555aaU) != 0xffffffffU)
    link_error ();
  return x;
}

unsigned long long int
f9 (unsigned long long int x)
{
  x |= 0x55aa5aa544cc2211ULL;
  x = __builtin_bswap64 (x);
  if ((x | 0xeedd33bb5aa555aaULL) != 0xffffffffffffffffULL)
    link_error ();
  return x;
}
