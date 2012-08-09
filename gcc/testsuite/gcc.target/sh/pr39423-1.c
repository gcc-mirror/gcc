/* Check that displacement addressing is used for indexed addresses with a
   small offset, instead of re-calculating the index.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "add\t#1" } } */

int
test_00 (int tab[], int index)
{
  return tab[index + 1];
}

int
test_01 (short tab[], int index)
{
  return tab[index + 1];
}

int
test_02 (unsigned short tab[], int index)
{
  return tab[index + 1];
}

int
test_03 (long long tab[], int index)
{
  return (int)tab[index + 1];
}

void
test_04 (int tab[], int index, int val)
{
  tab[index + 1] = val;
}

void
test_05 (short tab[], int index, int val)
{
  tab[index + 1] = (short)val;
}

void
test_06 (unsigned short tab[], int index, int val)
{
  tab[index + 1] = (unsigned short)val;
}
