/* PR rtl-optimization/59461 */

/* { dg-do compile } */
/* { dg-options "-O2" } */

extern char zeb_test_array[10];

unsigned char ee_isdigit2(unsigned int i)
{
  unsigned char c = zeb_test_array[i];
  unsigned char retval;

  retval = ((c>='0') & (c<='9')) ? 1 : 0;
  return retval;
}

/* { dg-final { scan-assembler-not "and\t%" } } */
