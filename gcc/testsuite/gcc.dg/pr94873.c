/* PR rtl-optimization/94873 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O -fno-merge-constants -fno-split-wide-types -fno-tree-fre" } */

__attribute__((noipa)) void
foo (const char *p, int q)
{
  if (p[0] != '%' || p[1] != '0' || p[2] != '2' || p[3] != 'x' || p[4] != '\0')
    __builtin_abort ();
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  if ((unsigned char) q != 0x95)
    __builtin_abort ();
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  if ((unsigned char) q != 0)
    __builtin_abort ();
#endif
}

int
main ()
{
  union U { __int128 a; char b[sizeof (__int128)]; };
  char x = ((union U){ .a = 0xF4409395252B9560ULL}).b[1];
  for (unsigned i = 0; i < sizeof (x); i++)
    foo ("%02x", i[(volatile unsigned char *) &x]);
  return 0;
}
