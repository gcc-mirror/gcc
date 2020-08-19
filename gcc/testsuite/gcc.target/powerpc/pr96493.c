/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target power10_ok } */

/* Test local calls between pcrel and non-pcrel code.

   Despite the cpu=power10 option, the code generated here should just
   be plain powerpc64, even the necessary linker stubs.  */

int one = 1;

int __attribute__ ((target("cpu=power8"),noclone,noinline))
p8_func (int x)
{
  return x - one;
}

int __attribute__ ((target("cpu=power10"),noclone,noinline))
p10_func (int x)
{
  return p8_func (x);
}

int
main (void)
{
  return p10_func (1);
}
