/* { dg-do run } */
/* { dg-options "-O" } */
int
main (void)
{
  /* Test constant folding.  */
  extern void link_error (void);

  if (__builtin_bswap16(0xabcd) != 0xcdab)
    link_error ();

  if (__builtin_bswap32(0x89abcdef) != 0xefcdab89)
    link_error ();

  if (__builtin_bswap64(0x0123456789abcdefULL) != 0xefcdab8967452301ULL)
    link_error ();

  return 0;
}
