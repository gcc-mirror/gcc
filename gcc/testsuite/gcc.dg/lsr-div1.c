/* Test division by const int generates only one shift.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-rtl-combine-all" } */
/* { dg-options "-O2 -fdump-rtl-combine-all -mtune=cortex-a53" { target aarch64*-*-* } } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);

#define NOINLINE __attribute__((noinline))

static NOINLINE int
f1 (unsigned int n)
{
  return n % 0x33;
}

static NOINLINE int
f2 (unsigned int n)
{
  return n % 0x12;
}

int
main ()
{
  int a = 0xaaaaaaaa;
  int b = 0x55555555;
  int c;
  c = f1 (a);
  if (c != 0x11)
    abort ();
  c = f1 (b);
  if (c != 0x22)
    abort ();
  c = f2 (a);
  if (c != 0xE)
    abort ();
  c = f2 (b);
  if (c != 0x7)
    abort ();
  return 0;
}

/* Following replacement pattern of intger division by constant, GCC is expected
   to generate UMULL and (x)SHIFTRT.  This test checks that considering division
   by const 0x33, gcc generates a single LSHIFTRT by 37, instead of
   two - LSHIFTRT by 32 and LSHIFTRT by 5.  */

/* { dg-final { scan-rtl-dump "\\(set \\(subreg:DI \\(reg:SI" "combine" { target aarch64*-*-* } } } */
/* { dg-final { scan-rtl-dump "\\(lshiftrt:DI \\(reg:DI" "combine" { target aarch64*-*-* } } } */
/* { dg-final { scan-rtl-dump "\\(const_int 37 " "combine" { target aarch64*-*-* } } } */

/* Similarly, considering division by const 0x12, gcc generates a
   single LSHIFTRT by 34, instead of two - LSHIFTRT by 32 and LSHIFTRT by 2.  */

/* { dg-final { scan-rtl-dump "\\(const_int 34 " "combine" { target aarch64*-*-* } } } */

