// Test sequences that can use RISBG with a zeroed first operand.
// The tests here assume that RISBLG isn't available.

/* Tests ported from the Llvm testsuite. */

/* { dg-do compile { target s390x-*-* } } */
/* { dg-options "-O3 -march=z10 -mzarch -fno-asynchronous-unwind-tables" } */

#define i64 signed long long
#define ui64 unsigned long long
#define i32 signed int
#define ui32 unsigned int
#define i8 signed char
#define ui8 unsigned char

// Test an extraction of bit 0 from a right-shifted value.
i32 f1 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f1:\n\trisbg\t%r2,%r2,64-1,128\\\+63,53\\\+1" } } */
  i32 v_shr = ((ui32)v_foo) >> 10;
  i32 v_and = v_shr & 1;
  return v_and;
}

// ...and again with i64.
i64 f2 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f2:\n\trisbg\t%r2,%r2,64-1,128\\\+63,53\\\+1" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f2:\n\trisbg\t%r3,%r3,64-1,128\\\+63,53\\\+1\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_shr = ((ui64)v_foo) >> 10;
  i64 v_and = v_shr & 1;
  return v_and;
}

// Test an extraction of other bits from a right-shifted value.
i32 f3 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f3:\n\trisbg\t%r2,%r2,60,128\\\+61,64-22" } } */
  i32 v_shr = ((ui32)v_foo) >> 22;
  i32 v_and = v_shr & 12;
  return v_and;
}

// ...and again with i64.
i64 f4 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f4:\n\trisbg\t%r2,%r2,60,128\\\+61,64-22" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f4:\n\trisbg\t%r3,%r3,60,128\\\+61,64-22\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_shr = ((ui64)v_foo) >> 22;
  i64 v_and = v_shr & 12;
  return v_and;
}

// Test an extraction of most bits from a right-shifted value.
// The range should be reduced to exclude the zeroed high bits.
i32 f5 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f5:\n\trisbg\t%r2,%r2,34,128\\\+60,64-2" } } */
  i32 v_shr = ((ui32)v_foo) >> 2;
  i32 v_and = v_shr & -8;
  return v_and;
}

// ...and again with i64.
i64 f6 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f6:\n\trisbg\t%r2,%r2,2,128\\\+60,64-2" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f6:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\trisbg\t%r2,%r3,2,128\\\+60,64-2" { target { ! lp64 } } } } */
  i64 v_shr = ((ui64)v_foo) >> 2;
  i64 v_and = v_shr & -8;
  return v_and;
}

// Try the next value up (mask ....1111001).  This needs a separate shift
// and mask.
i32 f7 (i32 v_foo)
{
  /* Should be
     { dg-final { scan-assembler "f7:\n\tsrl\t%r2,2\n\tnill\t%r2,65529" { xfail { lp64 } } } }
     but because a zeroextend is merged into the pattern it is actually
     { dg-final { scan-assembler "f7:\n\tsrl\t%r2,2\n\tlgfi\t%r1,1073741817\n\tngr\t%r2,%r1" { target { lp64 } } } }
     { dg-final { scan-assembler "f7:\n\tsrl\t%r2,2\n\tnill\t%r2,65529" { target { ! lp64 } } } } */
  i32 v_shr = ((ui32)v_foo) >> 2;
  i32 v_and = v_shr & -7;
  return v_and;
}

// ...and again with i64.
i64 f8 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f8:\n\tsrlg\t%r2,%r2,2\n\tnill\t%r2,65529" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f8:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tsrlg\t%r2,%r3,2\n\tnill\t%r2,65529" { target { ! lp64 } } } } */
  i64 v_shr = ((ui64)v_foo) >> 2;
  i64 v_and = v_shr & -7;
  return v_and;
}

// Test an extraction of bits from a left-shifted value.  The range should
// be reduced to exclude the zeroed low bits.
i32 f9 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f9:\n\trisbg\t%r2,%r2,56,128\\\+61,2" } } */
  i32 v_shr = v_foo << 2;
  i32 v_and = v_shr & 255;
  return v_and;
}

// ...and again with i64.
i64 f10 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f10:\n\trisbg\t%r2,%r2,56,128\\\+61,2" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f10:\n\trisbg\t%r3,%r3,56,128\\\+61,2\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_shr = v_foo << 2;
  i64 v_and = v_shr & 255;
  return v_and;
}

// Try a wrap-around mask (mask ....111100001111).  This needs a separate shift
// and mask.
i32 f11 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f11:\n\tsll\t%r2,2\n\tnill\t%r2,65295" } } */
  i32 v_shr = v_foo << 2;
  i32 v_and = v_shr & -241;
  return v_and;
}

// ...and again with i64.
i64 f12 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f12:\n\tsllg\t%r2,%r2,2\n\tnill\t%r2,65295" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f12:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tsllg\t%r2,%r3,2\n\tnill\t%r2,65295" { target { ! lp64 } } } } */
  i64 v_shr = v_foo << 2;
  i64 v_and = v_shr & -241;
  return v_and;
}

// Test an extraction from a rotated value, no mask wraparound.
// This is equivalent to the lshr case, because the bits from the
// shl are not used.
i32 f13 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f13:\n\trisbg\t%r2,%r2,56,128\\\+60,32\\\+14" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f13:\n\trll\t%r2,%r2,14\n\tnilf\t%r2,248" { target { ! lp64 } } } } */
  i32 v_parta = v_foo << 14;
  i32 v_partb = ((ui32)v_foo) >> 18;
  i32 v_rotl = v_parta | v_partb;
  i32 v_and = v_rotl & 248;
  return v_and;
}

// ...and again with i64.
i64 f14 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f14:\n\trisbg\t%r2,%r2,56,128\\\+60,14" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f14:\n\trisbg\t%r3,%r2,56,128\\\+60,46\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_parta = v_foo << 14;
  i64 v_partb = ((ui64)v_foo) >> 50;
  i64 v_rotl = v_parta | v_partb;
  i64 v_and = v_rotl & 248;
  return v_and;
}

// Try a case in which only the bits from the shl are used.
i32 f15 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f15:\n\trisbg\t%r2,%r2,47,128\\\+49,14" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f15:\n\trll\t%r2,%r2,14\n\tnilf\t%r2,114688" { target { ! lp64 } } } } */
  i32 v_parta = v_foo << 14;
  i32 v_partb = ((ui32)v_foo) >> 18;
  i32 v_rotl = v_parta | v_partb;
  i32 v_and = v_rotl & 114688;
  return v_and;
}

// ...and again with i64.
i64 f16 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f16:\n\trisbg\t%r2,%r2,47,128\\\+49,14" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f16:\n\trisbg\t%r3,%r3,47,128\\\+49,14\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_parta = v_foo << 14;
  i64 v_partb = ((ui64)v_foo) >> 50;
  i64 v_rotl = v_parta | v_partb;
  i64 v_and = v_rotl & 114688;
  return v_and;
}

// Test a 32-bit rotate in which both parts of the OR are needed.
// This needs a separate shift and mask.
i32 f17 (i32 v_foo)
{
  /* Should be
     { dg-final { scan-assembler "f17:\n\trll\t%r2,%r2,4\n\tnilf\t%r2,126" { xfail { lp64 } } } }
     but because a zeroextend is merged into the pattern it is actually
     { dg-final { scan-assembler "f17:\n\trll\t%r2,%r2,4\n\trisbg\t%r2,%r2,57,128\\\+62,0" { target { lp64 } } } }
     { dg-final { scan-assembler "f17:\n\trll\t%r2,%r2,4\n\tnilf\t%r2,126" { target { ! lp64 } } } } */
  i32 v_parta = v_foo << 4;
  i32 v_partb = ((ui32)v_foo) >> 28;
  i32 v_rotl = v_parta | v_partb;
  i32 v_and = v_rotl & 126;
  return v_and;
}

// ...and for i64, where RISBG should do the rotate too.
i64 f18 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f18:\n\trisbg\t%r2,%r2,57,128\\\+62,4" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f18:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tlhi\t%r2,0\n\trisbg\t%r3,%r3,57,128\\\+62,4" { target { ! lp64 } } } } */
  i64 v_parta = v_foo << 4;
  i64 v_partb = ((ui64)v_foo) >> 60;
  i64 v_rotl = v_parta | v_partb;
  i64 v_and = v_rotl & 126;
  return v_and;
}

// Test an arithmetic shift right in which some of the sign bits are kept.
// This needs a separate shift and mask.
i32 f19 (i32 v_foo)
{
  /* Should be
     { dg-final { scan-assembler "f19:\n\tsra\t%r2,28\n\tnilf\t%r2,30" { xfail { lp64 } } } }
     but because a zeroextend is merged into the pattern it is actually
     { dg-final { scan-assembler "f19:\n\tsra\t%r2,28\n\trisbg\t%r2,%r2,59,128\\\+62,0" { target { lp64 } } } }
     { dg-final { scan-assembler "f19:\n\tsra\t%r2,28\n\tnilf\t%r2,30" { target { ! lp64 } } } } */
  i32 v_shr = v_foo >> 28;
  i32 v_and = v_shr & 30;
  return v_and;
}

// ...and again with i64.  In this case RISBG is the best way of doing the AND.
i64 f20 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f20:\n\tsrag\t%r2,%r2,60\n\trisbg\t%r2,%r2,59,128\\\+62,0" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f20:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tlhi\t%r2,0\n\tsrag\t%r3,%r3,60\n\tnilf\t%r3,30" { target { ! lp64 } } } } */
  i64 v_shr = v_foo >> 60;
  i64 v_and = v_shr & 30;
  return v_and;
}

// Now try an arithmetic right shift in which the sign bits aren't needed.
// Note: Unlike Llvm, Gcc replaces the ashrt with a lshrt in any case, using
// a risbg pattern without ashrt.
i32 f21 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f21:\n\trisbg\t%r2,%r2,60,128\\\+62,64-28" } } */
  i32 v_shr = v_foo >> 28;
  i32 v_and = v_shr & 14;
  return v_and;
}

// ...and again with i64.
i64 f22 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f22:\n\trisbg\t%r2,%r2,60,128\\\+62,64-60" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f22:\n\trisbg\t%r3,%r2,60,128\\\+62,64-28\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_shr = v_foo >> 60;
  i64 v_and = v_shr & 14;
  return v_and;
}

// Check that we use RISBG for shifted values even if the AND is a
// natural zero extension.
i64 f23 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f23:\n\trisbg\t%r2,%r2,64-8,128\\\+63,54\\\+8" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f23:\n\trisbg\t%r3,%r3,64-8,128\\\+63,54\\\+8\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_shr = ((ui64)v_foo) >> 2;
  i64 v_and = v_shr & 255;
  return v_and;
}

// Test a case where the AND comes before a rotate.  This needs a separate
// mask and rotate.
i32 f24 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f24:\n\tnilf\t%r2,254\n\trll\t%r2,%r2,29" } } */
  i32 v_and = v_foo & 254;
  i32 v_parta = ((ui32)v_and) >> 3;
  i32 v_partb = v_and << 29;
  i32 v_rotl = v_parta | v_partb;
  return v_rotl;
}

// ...and again with i64, where a single RISBG is enough.
i64 f25 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f25:\n\trisbg\t%r2,%r2,57,128\\\+59,3" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f25:\n\trisbg\t%r3,%r3,57,128\\\+59,3\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_and = v_foo & 14;
  i64 v_parta = v_and << 3;
  i64 v_partb = ((ui64)v_and) >> 61;
  i64 v_rotl = v_parta | v_partb;
  return v_rotl;
}

// Test a wrap-around case in which the AND comes before a rotate.
// This again needs a separate mask and rotate.
i32 f26 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f26:\n\tnill\t%r2,65487\n\trll\t%r2,%r2,5" } } */
  i32 v_and = v_foo & -49;
  i32 v_parta = v_and << 5;
  i32 v_partb = ((ui32)v_and) >> 27;
  i32 v_rotl = v_parta | v_partb;
  return v_rotl;
}

// ...and again with i64, where a single RISBG is OK.
i64 f27 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f27:\n\trisbg\t%r2,%r2,55,128\\\+52,5" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f27:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\trisbg\t%r2,%r3,55,128\\\+52,5" { target { ! lp64 } } } } */
  i64 v_and = v_foo & -49;
  i64 v_parta = v_and << 5;
  i64 v_partb = ((ui64)v_and) >> 59;
  i64 v_rotl = v_parta | v_partb;
  return v_rotl;
}

// Test a case where the AND comes before a shift left.
i32 f28 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f28:\n\trisbg\t%r2,%r2,32,128\\\+45,17" } } */
  i32 v_and = v_foo & 32766;
  i32 v_shl = v_and << 17;
  return v_shl;
}

// ...and again with i64.
i64 f29 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f29:\n\trisbg\t%r2,%r2,0,128\\\+13,49" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f29:\n\trisbg\t%r\[23\],%r3,0,128\\\+13,49\n\tlr\t%r\[23\],%r\[32\]\n\tsrlg\t%r2,%r2" { target { ! lp64 } } } } */
  i64 v_and = v_foo & 32766;
  i64 v_shl = v_and << 49;
  return v_shl;
}

// Test the next shift up from f28, in which the mask should get shortened.
i32 f30 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f30:\n\trisbg\t%r2,%r2,32,128\\\+44,18" } } */
  i32 v_and = v_foo & 32766;
  i32 v_shl = v_and << 18;
  return v_shl;
}

// ...and again with i64.
i64 f31 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f31:\n\trisbg\t%r2,%r2,0,128\\\+12,50" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f31:\n\trisbg\t%r\[23\],%r3,0,128\\\+12,50\n\tlr\t%r\[23\],%r\[32\]\n\tsrlg\t%r2,%r2" { target { ! lp64 } } } } */
  i64 v_and = v_foo & 32766;
  i64 v_shl = v_and << 50;
  return v_shl;
}

// Test a wrap-around case in which the shift left comes after the AND.
// We can't use RISBG for the shift in that case.
i32 f32 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f32:\n\tsll\t%r2,10\n\tnill\t%r2,58368" } } */
  i32 v_and = v_foo & -7;
  i32 v_shl = v_and << 10;
  return v_shl;
}

// ...and again with i64.
i64 f33 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f33:\n\tsllg\t%r2,%r2,10\n\tnill\t%r2,58368" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f33:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tsllg\t%r2,%r3,10\n\tnill\t%r2,58368" { target { ! lp64 } } } } */
  i64 v_and = v_foo & -7;
  i64 v_shl = v_and << 10;
  return v_shl;
}

// Test a case where the AND comes before a shift right.
i32 f34 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f34:\n\trisbg\t%r2,%r2,64-7,128\\\+63,48\\\+7" } } */
  i32 v_and = v_foo & 65535;
  i32 v_shl = ((ui32)v_and) >> 9;
  return v_shl;
}

// ...and again with i64.
i64 f35 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f35:\n\trisbg\t%r2,%r2,64-7,128\\\+63,48\\\+7" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f35:\n\trisbg\t%r3,%r3,64-7,128\\\+63,48\\\+7\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i64 v_and = v_foo & 65535;
  i64 v_shl = ((ui64)v_and) >> 9;
  return v_shl;
}

// Test a wrap-around case where the AND comes before a shift right.
// We can't use RISBG for the shift in that case.
i32 f36 (i32 v_foo)
{
  /* { dg-final { scan-assembler "f36:\n\tsrl\t%r2,1\n\tlgfi\t%r1,2147483635\n\tngr\t%r2,%r1" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f36:\n\tsrl\t%r2,1\n\tnilf\t%r2,2147483635" { target { ! lp64 } } } } */
  i32 v_and = v_foo & -25;
  i32 v_shl = ((ui32)v_and) >> 1;
  return v_shl;
}

// ...and again with i64.
i64 f37 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f37:\n\(\t.*\n\)*\tsrlg\t%r2,%r2,1\n\tng\t%r2," { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f37:\n\(\t.*\n\)*\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tsrlg\t%r2,%r3,1\n\tng\t%r2," { target { ! lp64 } } } } */
  i64 v_and = v_foo & -25;
  i64 v_shl = ((ui64)v_and) >> 1;
  return v_shl;
}

// Test a combination involving a large ASHR and a shift left.  We can't
// use RISBG there.
i64 f38 (i64 v_foo)
{
  /* { dg-final { scan-assembler "f38:\n\tsrag\t%r2,%r2,32\n\tsllg\t%r2,%r2,5" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f38:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tsrag\t%r2,%r3,32\n\tsllg\t%r2,%r2,5" { target { ! lp64 } } } } */
  i64 v_ashr = v_foo >> 32;
  i64 v_shl = v_ashr << 5;
  return v_shl;
}

// Try a similar thing in which no shifted sign bits are kept.
i64 f39 (i64 v_foo, i64 *v_dest)
{
  /* { dg-final { scan-assembler "f39:\n\tsrag\t%r2,%r2,35\n\(\t.*\n\)*\trisbg\t%r2,%r2,33,128\\\+61,2" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f39:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tlhi\t%r2,0\n\tsrag\t%r3,%r3,35\n\(\t.*\n\)*\trisbg\t%r3,%r3,33,128\\\+61,2" { target { ! lp64 } } } } */
  i64 v_ashr = v_foo >> 35;
  *v_dest = v_ashr;
  i64 v_shl = v_ashr << 2;
  i64 v_and = v_shl & 2147483647;
  return v_and;
}

// ...and again with the next highest shift value, where one sign bit is kept.
i64 f40 (i64 v_foo, i64 *v_dest)
{
  /* { dg-final { scan-assembler "f40:\n\tsrag\t%r2,%r2,36\n\(\t.*\n\)*\trisbg\t%r2,%r2,33,128\\\+61,2" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f40:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\tlhi\t%r2,0\n\tsrag\t%r3,%r3,36\n\(\t.*\n\)*\trisbg\t%r3,%r3,33,128\\\+61,2" { target { ! lp64 } } } } */
  i64 v_ashr = v_foo >> 36;
  *v_dest = v_ashr;
  i64 v_shl = v_ashr << 2;
  i64 v_and = v_shl & 2147483647;
  return v_and;
}

// Check a case where the result is zero-extended.
i64 f41 (i32 v_a)
{
  /* { dg-final { scan-assembler "f41:\n\trisbg\t%r2,%r2,64-28,128\\\+63,34\\\+28" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f41:\n\trisbg\t%r3,%r2,64-28,128\\\+63,34\\\+28\n\tlhi\t%r2,0" { target { ! lp64 } } } } */
  i32 v_shl = v_a << 2;
  i32 v_shr = ((ui32)v_shl) >> 4;
  i64 v_ext = (ui64)v_shr;
  return v_ext;
}

// In this case the sign extension is converted to a pair of 32-bit shifts,
// which is then extended to 64 bits.  We previously used the wrong bit size
// when testing whether the shifted-in bits of the shift right were significant.
typedef struct { ui64 pad : 63; ui8 a : 1; } t42;
i64 f42 (t42 v_x)
{
  /* { dg-final { scan-assembler "f42:\n\tsllg\t%r2,%r2,63\n\tsrag\t%r2,%r2,63\n\tllgcr\t%r2,%r2" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f42:\n\tsllg\t%r3,%r3,63\n\tlhi\t%r2,0\n\tsrag\t%r3,%r3,63\n\tllcr\t%r3,%r3" { target { ! lp64 } } } } */
  ui8 a = v_x.a << 7;
  i8 ext = ((i8)a) >> 7;
  i64 ext2 = (ui64)(ui8)ext;
  return ext2;
}

// Check that we get the case where a 64-bit shift is used by a 32-bit and.
i32 f43 (i64 v_x)
{
  /* { dg-final { scan-assembler "f43:\n\trisbg\t%r2,%r2,32,128\\\+61,64-12" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f43:\n\trisbg\t%r3,%r2,0,0\\\+32-1,64-0-32\n\trisbg\t%r2,%r3,32,128\\\+61,64-12" { target { ! lp64 } } } } */
  i64 v_shr3 = ((ui64)v_x) >> 12;
  i32 v_shr3_tr = (ui32)v_shr3;
  i32 v_conv = v_shr3_tr & -4;
  return v_conv;
}

// Check that we don't get the case where the 32-bit and mask is not contiguous
i32 f44 (i64 v_x)
{
  /* { dg-final { scan-assembler "f44:\n\tsrlg\t%r2,%r2,12" { target { lp64 } } } } */
  /* { dg-final { scan-assembler "f44:\n\tsrlg\t%r2,%r3,12\n\tnilf\t%r2,10" { target { ! lp64 } } } } */
  i64 v_shr4 = ((ui64)v_x) >> 12;
  i32 v_conv = (ui32)v_shr4;
  i32 v_and = v_conv & 10;
  return v_and;
}
