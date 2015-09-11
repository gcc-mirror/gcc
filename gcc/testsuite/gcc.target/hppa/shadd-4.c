/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-times "sh.add" 1 } }  */
unsigned int
oof (int uid)
{
  return (174 << 7) + uid;
}
