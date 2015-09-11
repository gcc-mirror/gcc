/* { dg-do compile } */
/* { dg-options " -O2" }  */
/* { dg-require-effective-target arm_thumb2_ok } */

#define MASK 0xff00ff
void maskdata (int * data, int len)
{
   int i = len;
   for (; i > 0; i -= 2)
    {
      data[i] &= MASK;
      data[i + 1] &= MASK;
    }
}
/* { dg-final { scan-assembler-not "65280" } } */
