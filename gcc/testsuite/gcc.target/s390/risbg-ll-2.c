// Test sequences that can use RISBG with a normal first operand.

/* Tests ported from the Llvm testsuite. */

/* { dg-do compile { target s390x-*-* } } */
/* { dg-options "-O3 -march=z10 -mzarch -fno-asynchronous-unwind-tables" }  */

#define i64 signed long long
#define ui64 unsigned long long
#define i32 signed int
#define ui32 unsigned int

// Test a case with two ANDs.
i32 f1 (i32 v_a, i32 v_b)
{
  /* { dg-final { scan-assembler "f1:\n\trisbg\t%r2,%r3,60,62,0" } } */
  i32 v_anda = v_a & -15;
  i32 v_andb = v_b & 14;
  i32 v_or = v_anda | v_andb;
  return v_or;
}

// ...and again with i64.
i64 f2 (i64 v_a, i64 v_b)
{
  /* { dg-final { scan-assembler "f2:\n\trisbg\t%r2,%r3,60,62,0" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f2:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\(\t.*\n\)*\trisbg\t%r\[23\],%r5,60,62,0" { target { ! lp64 } } } } */
  i64 v_anda = v_a & -15;
  i64 v_andb = v_b & 14;
  i64 v_or = v_anda | v_andb;
  return v_or;
}

// Test a case with two ANDs and a shift.
i32 f3 (i32 v_a, i32 v_b)
{
  /* { dg-final { scan-assembler "f3:\n\trisbg\t%r2,%r3,64-4,63,4\\\+52" } } */
  i32 v_anda = v_a & -16;
  i32 v_shr = ((ui32)v_b) >> 8;
  i32 v_andb = v_shr & 15;
  i32 v_or = v_anda | v_andb;
  return v_or;
}

// ...and again with i64.
i64 f4 (i64 v_a, i64 v_b)
{
  /* { dg-final { scan-assembler "f4:\n\trisbg\t%r2,%r3,60,60\\\+4-1,128-60-4-8" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f4:\n\(\t.*\n\)*\trisbg\t%r5,%r5,64-4,128\\\+63,52\\\+4" { target { ! lp64 } } } } */
  i64 v_anda = v_a & -16;
  i64 v_shr = ((ui64)v_b) >> 8;
  i64 v_andb = v_shr & 15;
  i64 v_or = v_anda | v_andb;
  return v_or;
}

// Test a case with a single AND and a left shift.
i32 f5 (i32 v_a, i32 v_b)
{
  /* { dg-final { scan-assembler "f5:\n\trisbg\t%r2,%r3,32,64-10-1,10" } } */
  i32 v_anda = v_a & 1023;
  i32 v_shlb = v_b << 10;
  i32 v_or = v_anda | v_shlb;
  return v_or;
}

// ...and again with i64.
i64 f6 (i64 v_a, i64 v_b)
{
  /* { dg-final { scan-assembler "f6:\n\trisbg\t%r2,%r3,0,64-10-1,10" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f6:\n\trisbg\t%r5,%r4,0,0\\\+32-1,64-0-32\n\(\t.*\n\)*\trisbg\t%r\[23\],%r5,0,64-10-1,10" { target { ! lp64 } } } } */
  i64 v_anda = v_a & 1023;
  i64 v_shlb = v_b << 10;
  i64 v_or = v_anda | v_shlb;
  return v_or;
}

// Test a case with a single AND and a right shift.
i32 f7 (i32 v_a, i32 v_b)
{
  /* { dg-final { scan-assembler "f7:\n\trisbg\t%r2,%r3,32\\\+8,63,64-8" } } */
  i32 v_anda = v_a & -16777216;
  i32 v_shrb = ((ui32)v_b) >> 8;
  i32 v_or = v_anda | v_shrb;
  return v_or;
}

// ...and again with i64.
i64 f8 (i64 v_a, i64 v_b)
{
  /* { dg-final { scan-assembler "f8:\n\trisbg\t%r2,%r3,8,63,64-8" { target { lp64 } } } } */
  /* With -m31 risbg is not really useful here, so do not test for it.  */
  i64 v_anda = v_a & -72057594037927936;
  i64 v_shrb = ((ui64)v_b) >> 8;
  i64 v_or = v_anda | v_shrb;
  return v_or;
}

// Check that we can get the case where a 64-bit shift feeds a 32-bit or of
// ands with complement masks.
i32 f9 (i64 v_x, i32 v_y)
{
  /* { dg-final { scan-assembler "f9:\n\trisbg\t%r3,%r2,48,63,64-48" { target { lp64 } }} } */
  /* { dg-final { scan-assembler "f9:\n\trisbg\t%r4,%r2,32\\+16,63,64-16" { target { ! lp64 } }} } */
  i64 v_shr6 = ((ui64)v_x) >> 48;
  i32 v_conv = (ui32)v_shr6;
  i32 v_and1 = v_y & -65536;
  i32 v_or = v_conv | v_and1;
  return v_or;
}

// Check that we don't get the case where a 64-bit shift feeds a 32-bit or of
// ands with incompatible masks.
i32 f10 (i64 v_x, i32 v_y)
{
  /* { dg-final { scan-assembler "f10:\n\tsrlg\t%r2,%r2,48\n\trosbg\t%r2,%r3,32,39,0" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f10:\n\tnilf\t%r4,4278190080\n\trosbg\t%r4,%r2,32\\\+16,63,64-16" { target { ! lp64 } } } } */
  i64 v_shr6 = ((ui64)v_x) >> 48;
  i32 v_conv = (ui32)v_shr6;
  i32 v_and1 = v_y & -16777216;
  i32 v_or = v_conv | v_and1;
  return v_or;
}
