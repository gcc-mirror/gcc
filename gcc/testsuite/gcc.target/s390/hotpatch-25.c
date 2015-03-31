/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

typedef long (*fn_t)(void);

__attribute__ ((hotpatch(1,2)))
fn_t outer(void)
{
  __attribute__ ((hotpatch(4,8)))
  long nested1(void)
  {
    __attribute__ ((hotpatch(16,32)))
    long nested2(void)
    {
      return 2;
    }
    return (long)(void *)nested2;
  }

  return nested1;
}

/* { dg-final { scan-assembler "pre-label.*(1 halfwords)" } } */
/* { dg-final { scan-assembler "pre-label.*(4 halfwords)" } } */
/* { dg-final { scan-assembler "pre-label.*(16 halfwords)" } } */
/* { dg-final { scan-assembler "post-label.*(2 halfwords)" } } */
/* { dg-final { scan-assembler "post-label.*(8 halfwords)" } } */
/* { dg-final { scan-assembler "post-label.*(32 halfwords)" } } */
/* { dg-final { scan-assembler-times "alignment for hotpatch" 3 } } */
/* { dg-final { scan-assembler-times "\.align\t8" 6 } } */
/* { dg-final { scan-assembler "nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr.*\n.*nopr" } } */
