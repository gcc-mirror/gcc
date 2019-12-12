/* Check that we use the scalar variants of vector compares.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13 -fno-asynchronous-unwind-tables" } */

int
eq (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a == b;
}

/* { dg-final { scan-assembler "eq:\n\[^:\]*\twfcdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochie\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "eq:\n\[^:\]*\twfcdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghie\t%r2,1" { target lp64 } } } */

int
ne (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a != b;
}

/* { dg-final { scan-assembler "ne:\n\[^:\]*\twfcdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochine\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "ne:\n\[^:\]*\twfcdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghine\t%r2,1" { target lp64 } } } */

int
gt (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a > b;
}

/* { dg-final { scan-assembler "gt:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochih\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "gt:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghih\t%r2,1" { target lp64 } } } */

int
ge (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a >= b;
}

/* { dg-final { scan-assembler "ge:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochihe\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "ge:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghihe\t%r2,1" { target lp64 } } } */

int
lt (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a < b;
}

/* { dg-final { scan-assembler "lt:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochil\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "lt:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghil\t%r2,1" { target lp64 } } } */

int
le (double a, double b)
{
  asm ("" : : :
       "f0", "f1",  "f2",  "f3",  "f4" , "f5",  "f6",  "f7",
       "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15");
  return a <= b;
}

/* { dg-final { scan-assembler "le:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlochile\t%r2,1" { target { ! lp64 } } } } */
/* { dg-final { scan-assembler "le:\n\[^:\]*\twfkdb\t%v\[0-9\]*,%v\[0-9\]*\n\t\[^:\]+\tlocghile\t%r2,1" { target lp64 } } } */
