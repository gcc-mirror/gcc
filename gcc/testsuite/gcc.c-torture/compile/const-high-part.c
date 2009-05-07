/* { dg-require-effective-target int32plus } */
/* { dg-require-effective-target size32plus } */

char *buf;
int buflen;

inline int
sub (int length)
{
  if (length <= buflen)
    buf[length] = '\0';
  return 0;
}

int
sub2 (void)
{
  return sub (0x7fffffff);
}
