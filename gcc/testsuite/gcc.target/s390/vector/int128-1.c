/* Check that vaq/vsq are used for int128 operations.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */


const __int128 c = (__int128)0x0123456789abcd55 + ((__int128)7 << 64);


__int128
addreg(__int128 a, __int128 b)
{
  return a + b;
}

__int128
addconst(__int128 a)
{
  return a + c;
}

__int128
addmem(__int128 *a, __int128_t *b)
{
  return *a + *b;
}

__int128
subreg(__int128 a, __int128 b)
{
  return a - b;
}

__int128
subconst(__int128 a)
{
  return a - c; /* This becomes vaq as well.  */
}

__int128
submem(__int128 *a, __int128_t *b)
{
  return *a - *b;
}

/* { dg-final { scan-assembler-times "vaq" 4 } } */
/* { dg-final { scan-assembler-times "vsq" 2 } } */
