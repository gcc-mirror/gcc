/* Test a situation in which an M6 instruction (mdcutssi) and M4 instruction
   (mqmulhu) can be issued together.  */
/* { dg-options "-mcpu=fr450" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int
main ()
{
  __MQMULHU (0, 0x0001001101111111ULL, 0x0001000200030004ULL);
  __MQMULHU (8, 0x0002002202222222ULL, 0x0004000400040004ULL);
  if (__MDCUTSSI (0, 8)
      + __MDCUTSSI (2, 8)
      + __MDCUTSSI (8, 8)
      + __MDCUTSSI (10, 8)
      != (0x0000000100000022ULL + 0x0000033300004444ULL
	  + 0x0000000800000088ULL + 0x0000088800008888ULL))
    abort ();

  exit (0);
}
