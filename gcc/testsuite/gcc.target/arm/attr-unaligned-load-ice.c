/* PR target/68617
   Verify that unaligned_access is correctly with attribute target.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6" } } */
/* { dg-require-effective-target arm_arch_v6_ok } */
/* { dg-options "-Os -mfloat-abi=softfp -mtp=soft" } */
/* { dg-add-options arm_arch_v6 } */

long __attribute__((target("thumb")))
foo (char *s, long size, int unsigned_p)
{
  long x;
  unsigned char *p = (unsigned char *) s;
  switch (size)
    {
    case 4:
      x = ((long) p[3] << 24) | ((long) p[2] << 16) | (p[1] << 8) | p[0];
      return x;
    }
}
