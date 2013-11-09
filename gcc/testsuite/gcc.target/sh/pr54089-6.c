/* Check that the rotr and rotl instructions are generated.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "rotr" 2 } } */
/* { dg-final { scan-assembler-times "rotl" 3 } } */

int
test_00 (int a)
{
  return (a << 1) | ((a >> 31) & 1);
}

int
test_01 (int a)
{
  return (a << 1) | ((unsigned int)a >> 31);
}

int
test_02 (int a)
{
  return ((unsigned int)a >> 1) | (a << 31);
}

int
test_03 (int a)
{
  return ((a >> 1) & 0x7FFFFFFF) | (a << 31);
}

int
test_04 (int a)
{
  return a + a + ((a >> 31) & 1);
}
