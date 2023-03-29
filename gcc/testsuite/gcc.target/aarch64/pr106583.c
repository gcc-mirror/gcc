/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps" } */

long f1 (void)
{
  return 0x7efefefefefefeff;
}

long f2 (void)
{
  return 0x12345678aaaaaaaa;
}

long f3 (void)
{
  return 0x1234cccccccc5678;
}

long f4 (void)
{
  return 0x7777123456787777;
}

long f5 (void)
{
  return 0x5555555512345678;
}

long f6 (void)
{
  return 0x1234bbbb5678bbbb;
}

long f7 (void)
{
  return 0x4444123444445678;
}


/* { dg-final { scan-assembler-times {\tmovk\t} 14 } } */
/* { dg-final { scan-assembler-times {\tmov\t} 7 } } */
