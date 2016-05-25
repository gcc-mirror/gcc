/* Test division by const int generates only one shift.  */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-rtl-combine-all" } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);

#define NOINLINE __attribute__((noinline))

static NOINLINE int
f1 (int n)
{
  return n / 33;
}

static NOINLINE int
f2 (int n)
{
  return n / 77;
}

int
main ()
{
  int a = 0xaaaaaaaa;
  int b = 0x55555555;
  int c;
  c = f1 (a);
  if (c != 0xfd6a052c)
    abort ();
  c = f1 (b);
  if (c != 0x295FAD4)
    abort ();
  c = f2 (a);
  if (c != 0xfee44b5c)
    abort ();
  c = f2 (b);
  if (c != 0x11bb4a4)
    abort ();
  return 0;
}

/* Following replacement pattern of intger division by constant, GCC is expected
   to generate MULT and (x)SHIFTRT.  This test checks that considering division
   by const 33, gcc generates a single ASHIFTRT by 35, instead of two - LSHIFTRT
   by 32 and ASHIFTRT by 3.  */

/* { dg-final { scan-rtl-dump "\\(set \\(subreg:DI \\(reg:SI" "combine" { target aarch64*-*-* } } } */
/* { dg-final { scan-rtl-dump "\\(ashiftrt:DI \\(reg:DI" "combine" { target aarch64*-*-* } } } */
/* { dg-final { scan-rtl-dump "\\(const_int 35 " "combine" { target aarch64*-*-* } } } */

/* Similarly, considering division by const 77, gcc generates a single ASHIFTRT
   by 36, instead of two - LSHIFTRT by 32 and ASHIFTRT by 4.  */

/* { dg-final { scan-rtl-dump "\\(const_int 36 " "combine" { target aarch64*-*-* } } } */

