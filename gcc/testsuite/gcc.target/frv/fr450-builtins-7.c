/* Test that the code from fr450-builtins-6.c packs together an
   M4 and M5 instruction.  */
/* { dg-options "-O2 -mcpu=fr450" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler "mqmulhu.p\[^\t\]*\t*mrdacc" } } */
extern void abort (void);
extern void exit (int);

int
main ()
{
  __MQMULHU (0, 0x0011002200330044ULL, 0x0002000300040001ULL);
  __MQMULHU (8, 0x0100020003000400ULL, 0x0001000200030004ULL);

  /* 0x22 + 0x66 + 0xcc + 0x44 = 0x198 */
  /* 0x100 + 0x400 + 0x900 + 0x1000 = 0x1e00 */
  if (__MRDACC (0) + __MRDACC (1)
      + __MRDACC (2) + __MRDACC (3)
      + __MRDACC (8) + __MRDACC (9)
      + __MRDACC (10) + __MRDACC (11) != 0x1f98)
    abort ();

  exit (0);
}
