/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -mcmem" } */

struct some_struct
{
  unsigned char a;
};

unsigned char other_func (unsigned char);

unsigned char
some_function ()
{
  static struct some_struct ss __attribute__ ((section (".cmem")));
  static struct some_struct tt;

  ss.a = other_func (ss.a);
  tt.a = other_func (tt.a);

  return 0;
}

/* { dg-final { scan-assembler "xldb\\s+\[^\n\]*@ss" } } */
/* { dg-final { scan-assembler "xstb\\s+\[^\n\]*@ss" } } */
/* { dg-final { scan-assembler-not "xldb\\s+\[^\n\]*@tt" } } */
/* { dg-final { scan-assembler-not "xstb\\s+\[^\n\]*@tt" } } */
