/* { dg-do compile { target ia32 } } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fPIE" } */
/* { dg-final { scan-assembler-not "GOTOFF," } } */

static int *data[100];

void test (long a, long b)
{
  do
    {
      if( a < b )
        {
	  data[a] = data[b];
	  a++;
        }
    }
  while (a <= b);
}
