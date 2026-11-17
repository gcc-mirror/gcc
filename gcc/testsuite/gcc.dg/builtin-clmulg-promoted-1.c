/* Check that narrow __builtin_clmulg results stored in promoted registers
   are properly extended.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__ ((noipa)) unsigned short
f8 (unsigned char x, unsigned char y)
{
  return (unsigned char) __builtin_clmulg (x, y);
}

__attribute__ ((noipa)) unsigned int
f16 (unsigned short x, unsigned short y)
{
  return (unsigned short) __builtin_clmulg (x, y);
}

int
main ()
{
  if (f8 (0x7e, 0x9c) != 0xe8)
    __builtin_abort ();
  if (f16 (0xc8aa, 0x7616) != 0xa55c)
    __builtin_abort ();
  return 0;
}
