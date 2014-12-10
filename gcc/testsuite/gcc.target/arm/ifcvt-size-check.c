/* { dg-do assemble } */
/* { dg-options "-mthumb -Os " }  */
/* { dg-require-effective-target arm_thumb1_ok } */

int
test (unsigned char iov_len, int count, int i)
{
  unsigned char bytes = 0;
  if ((unsigned char) ((char) 127 - bytes) < iov_len)
    return 22;
  return 0;
}
/* { dg-final { object-size text <= 12 } } */
