/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-march=*" } { "-march=armv6k" } } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" } { "" } } */
/* { dg-require-effective-target arm_arch_v6k_thumb_ok } */
/* { dg-options "-mthumb -Os -mfloat-abi=softfp" } */
/* { dg-add-options arm_arch_v6k } */

long
get_number (char *s, long size, int unsigned_p)
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
