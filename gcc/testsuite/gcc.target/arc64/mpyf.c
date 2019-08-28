/* { dg-do compile } */
/* { dg-options "-O2" } */

/* "Checking if the result is zero" can safely use mpy.f.  The pattern of
   interest is:

   mpy.f   0,r0,r1
   mov.ne  r0,12345678  */
/* { dg-final { scan-assembler "mpy.f\\s+.*,r0,r1\n\\s*mov\.ne\\s+r0,12345678" } } */
int check_z_flag (int a, int b)
{
  if (a * b == 0)
    return a;
  else
    return 12345678;
}

/* "Checking if the result is negative" should not rely on .f notion of mpy,
   because in that case N will be only set if bit 63 is set.  The pattern
   of interest is something like:

   mpy_s   r1,r1,r0
   tst_s   r1,r1
   mov.n   r0,87654321 */
/* { dg-final { scan-assembler "mpy(_s)\\s+(\[^,\]*).*\n\\s*tst(_s)\\s+\\2,\\2\n\\s*mov\.n\\s+r0,87654321" } } */
int check_n_flag (int a, int b)
{
  if (a * b >= 0)
    return a;
  else
    return 87654321;
}

