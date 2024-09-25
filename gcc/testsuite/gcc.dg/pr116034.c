/* PR tree-optimization/116034 */
/* { dg-do run } */
/* { dg-options "-O1 -fno-strict-aliasing" } */

unsigned short int g;

static inline int
foo (_Complex unsigned short c)
{
  if (__SIZEOF_SHORT__ == 2)
    __builtin_memmove (&g, 1 + (char *) &c, 2);
  return g;
}

int
main ()
{
  if (__SIZEOF_SHORT__ == 2
      && __CHAR_BIT__ == 8
      && (foo (__BYTE_ORDER__ != __ORDER_BIG_ENDIAN__ ? 0x100 : 1)
	  != (__BYTE_ORDER__ != __ORDER_BIG_ENDIAN__ ? 1 : 0x100)))
    __builtin_abort ();
}
