/* Check that the tst Rm,Rn instruction is generated for QImode and HImode
   values loaded from memory.  If everything goes as expected we won't see
   any sign/zero extensions or and ops.  On SH2A we don't expect to see the
   movu insn.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "tst\tr" 8 } } */
/* { dg-final { scan-assembler-not "tst\t#255" } } */
/* { dg-final { scan-assembler-not "exts|extu|and|movu" } } */

int
test00 (char* a, char* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test01 (unsigned char* a, unsigned char* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test02 (short* a, short* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test03 (unsigned short* a, unsigned short* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test04 (char* a, short* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test05 (short* a, char* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test06 (int* a, char* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}

int
test07 (int* a, short* b, int c, int d)
{
  if (*a & *b)
    return c;
  return d;
}
