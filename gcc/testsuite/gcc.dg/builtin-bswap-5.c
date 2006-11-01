/* { dg-do run } */
/* { dg-options "-O" } */
int
main (void)
{
  /* Test constant folding.  */
  extern void link_error (void);

  if (__builtin_bswap32(0xaabbccdd) != 0xddccbbaa)
    link_error ();

  if (__builtin_bswap64(0x1122334455667788ULL) != 0x8877665544332211ULL)
    link_error ();

  return 0;
}
