/* revsh */
short swaps16 (short x)
{
  return __builtin_bswap16 (x);
}

extern short foos16 (short);

/* revshne */
short swaps16_cond (short x, int y)
{
  short z = x;
  if (y)
    z = __builtin_bswap16 (x);
  return foos16 (z);
}

/* rev16 */
unsigned short swapu16 (unsigned short x)
{
  return __builtin_bswap16 (x);
}

extern unsigned short foou16 (unsigned short);

/* rev16ne */
unsigned short swapu16_cond (unsigned short x, int y)
{
  unsigned short z = x;
  if (y)
    z = __builtin_bswap16 (x);
  return foou16 (z);
}

/* rev */
int swaps32 (int x) {
  return __builtin_bswap32 (x);
}

extern int foos32 (int);

/* revne */
int swaps32_cond (int x, int y)
{
  int z = x;
  if (y)
    z = __builtin_bswap32 (x);
  return foos32 (z);
}

/* rev */
unsigned int swapu32 (unsigned int x)
{
  return __builtin_bswap32 (x);
}

extern unsigned int foou32 (unsigned int);

/* revne */
unsigned int swapsu2 (unsigned int x, int y)
{
  int z = x;
  if (y)
    z = __builtin_bswap32 (x);
  return foou32 (z);
}
