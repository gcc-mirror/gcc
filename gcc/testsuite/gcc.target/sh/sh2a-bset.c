/* Testcase to check generation of a SH2A specific instruction
  'BSET #imm3,Rn'.  */
/* { dg-do assemble }  */
/* { dg-options "-O1" }  */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "bset"} }  */

struct a
{
  char a, b;
  short c;
};

/* This function generates the instruction "BSET #imm3,Rn" only
   on using optimization option "-O1" and above.  */

int
a2 ()
{
  volatile int j;
  volatile static struct a x = {1, 66, ~1}, y = {1, 2, ~2};

  if (j > 1)
    return (x.a == y.a && (x.b | 1) == y.b);
  if (j > 2)
    return (x.a == y.a && (x.b | 2) == y.b);
  if (j > 3)
    return (x.a == y.a && (x.b | 4) == y.b);
  if (j > 4)
    return (x.a == y.a && (x.b | 8) == y.b);
  if (j > 5)
    return (x.a == y.a && (x.b | 16) == y.b);
  if (j > 6)
    return (x.a == y.a && (x.b | 32) == y.b);
  if (j > 7)
    return (x.a == y.a && (x.b | 64) == y.b);
  if (j > 8)
    return (x.a == y.a && (x.b | 128) == y.b);
}

int
main ()
{
  volatile unsigned char x;

  x |= 0x1;
  x |= 0x2;
  x |= 0x4;
  x |= 0x8;
  x |= 0x16;
  x |= 0x32;
  x |= 0x64;
  x |= 0x128;

  if (!a2 ())
    return 0;
}
