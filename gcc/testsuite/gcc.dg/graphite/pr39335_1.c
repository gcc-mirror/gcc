/* { dg-options "-O2 -floop-interchange" } */

void crash_me(int num1, int num2, char * in, char * out)
{
 int i, j;
 for (j = 0; j < num1; j++)
   for (i = 0; i < num2; i++)
     *out++ = *in++;
}
