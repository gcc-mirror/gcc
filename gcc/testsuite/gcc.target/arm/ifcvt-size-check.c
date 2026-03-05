/* { dg-do assemble } */
/* { dg-require-effective-target arm_arch_v4t_thumb_ok } */
/* { dg-options "-Os" } */
/* { dg-add-options arm_arch_v4t_thumb } */

int
test (unsigned char iov_len, int count, int i)
{
  unsigned char bytes = 0;
  if ((unsigned char) ((char) 127 - bytes) < iov_len)
    return 22;
  return 0;
}
/* { dg-final { object-size text <= 12 } } */
