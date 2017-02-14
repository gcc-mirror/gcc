/* Machine description pattern tests.  */

/*
    { dg-options "-mzarch -save-temps" }

   Note that dejagnu-1.5.1 has a bug so that the action from the second dg-do
   always wins, even if the condition is false.  If this test is run on hardware
   older than z10 with a buggy dejagnu release, the execution part will fail.

    { dg-do assemble }
    { dg-do run { target { s390_useable_hw } } }

   Skip test if -O0, -march=z900, -march=z9-109 or -march=z9-ec is present on
   the command line:

    { dg-skip-if "" { *-*-* } { "-march=z9*" "-O0" } { "" } }

   Skip test if the -O or the -march= option is missing from the command line
   because it's difficult to detect the default:

    { dg-skip-if "" { *-*-* } { "*" } { "-O*" } }
    { dg-skip-if "" { *-*-* } { "*" } { "-march=*" } }
*/

__attribute__ ((noinline)) unsigned int
si_sll (unsigned int x)
{
  return (x << 1);
}

__attribute__ ((noinline)) unsigned int
si_srl (unsigned int x)
{
  return (x >> 2);
}

__attribute__ ((noinline)) unsigned int
rosbg_si_sll (unsigned int a, unsigned int b)
{
  return a | (b << 1);
}
/* { dg-final { scan-assembler-times "rosbg\t%r.,%r.,32,63-1,1" 1 } } */

__attribute__ ((noinline)) unsigned int
rosbg_si_srl (unsigned int a, unsigned int b)
{
  return a | (b >> 2);
}
/* { dg-final { scan-assembler-times "rosbg\t%r.,%r.,32\\+2,63,64-2" 1 } } */

__attribute__ ((noinline)) unsigned int
rxsbg_si_sll (unsigned int a, unsigned int b)
{
  return a ^ (b << 1);
}
/* { dg-final { scan-assembler-times "rxsbg\t%r.,%r.,32,63-1,1" 1 } } */

__attribute__ ((noinline)) unsigned int
rxsbg_si_srl (unsigned int a, unsigned int b)
{
  return a ^ (b >> 2);
}
/* { dg-final { scan-assembler-times "rxsbg\t%r.,%r.,32\\+2,63,64-2" 1 } } */

__attribute__ ((noinline)) unsigned long long
di_sll (unsigned long long x)
{
  return (x << 1);
}

__attribute__ ((noinline)) unsigned long long
di_srl (unsigned long long x)
{
  return (x >> 2);
}

__attribute__ ((noinline)) unsigned long long
rosbg_di_sll (unsigned long long a, unsigned long long b)
{
  return a | (b << 1);
}
/* { dg-final { scan-assembler-times "rosbg\t%r.,%r.,0,63-1,1" 1 } } */

__attribute__ ((noinline)) unsigned long long
rosbg_di_srl (unsigned long long a, unsigned long long b)
{
  return a | (b >> 2);
}
/* { dg-final { scan-assembler-times "rosbg\t%r.,%r.,2,63,64-2" 1 } } */

__attribute__ ((noinline)) unsigned long long
rxsbg_di_sll (unsigned long long a, unsigned long long b)
{
  return a ^ (b << 1);
}
/* { dg-final { scan-assembler-times "rxsbg\t%r.,%r.,0,63-1,1" 1 } } */

__attribute__ ((noinline)) unsigned long long
rxsbg_di_srl (unsigned long long a, unsigned long long b)
{
  return a ^ (b >> 2);
}
/* { dg-final { scan-assembler-times "rxsbg\t%r.,%r.,2,63,64-2" 1 } } */

int
main (void)
{
  /* SIMode */
  {
    unsigned int r;
    unsigned int a = 0x12488421u;
    unsigned int b = 0x88881111u;
    unsigned int csll = si_sll (b);
    unsigned int csrl = si_srl (b);

    r = rosbg_si_sll (a, b);
    if (r != (a | csll))
      __builtin_abort ();
    r = rosbg_si_srl (a, b);
    if (r != (a | csrl))
      __builtin_abort ();
    r = rxsbg_si_sll (a, b);
    if (r != (a ^ csll))
      __builtin_abort ();
    r = rxsbg_si_srl (a, b);
    if (r != (a ^ csrl))
      __builtin_abort ();
  }
  /* DIMode */
  {
    unsigned long long r;
    unsigned long long a = 0x1248357997538421lu;
    unsigned long long b = 0x8888444422221111lu;
    unsigned long long csll = di_sll (b);
    unsigned long long csrl = di_srl (b);

    r = rosbg_di_sll (a, b);
    if (r != (a | csll))
      __builtin_abort ();
    r = rosbg_di_srl (a, b);
    if (r != (a | csrl))
      __builtin_abort ();
    r = rxsbg_di_sll (a, b);
    if (r != (a ^ csll))
      __builtin_abort ();
    r = rxsbg_di_srl (a, b);
    if (r != (a ^ csrl))
      __builtin_abort ();
  }
  return 0;
}
