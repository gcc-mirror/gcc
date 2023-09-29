int test (void)
{
  const unsigned char *s = (const unsigned char *) "abc";
  const signed char *t = (const signed char *) "xyz";
  return s[1] + t[1];
}
