/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp_ok }
/* { dg-options "-mfpu=vfp" } */

int
test_sisf (float x)
{
  return (int)x;
}

short
test_hisf (float x)
{
  return (short)x;
}

float
test_sfsi (int x)
{
  return (float)x;
}

float
test_sfhi (short x)
{
  return (float)x;
}

short
test_hisi (int x)
{
  return (short)x;
}

int
test_sihi (short x)
{
  return (int)x;
}

/* {dg-final { scan-assembler-times {vcvt\.s32\.f32\ts[0-9]+,s[0-9]+} 2 }} */
/* {dg-final { scan-assembler-times {vcvt\.f32\.s32\ts[0-9]+,s[0-9]+} 2 }} */
/* {dg-final { scan-assembler-times {vmov\tr[0-9]+,s[0-9]+} 2 }} */
/* {dg-final { scan-assembler-times {vmov\ts[0-9]+,r[0-9]+} 2 }} */
/* {dg-final { scan-assembler-times {sxth\tr[0-9]+,r[0-9]+} 2 }} */
