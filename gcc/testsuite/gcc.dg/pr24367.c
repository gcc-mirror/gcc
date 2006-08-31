/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC -funroll-loops" } */

char *
test (const char *parent, const char *child)
{
  static char rtn_path[1024];
  char *s = rtn_path;
  char *s_end = rtn_path + sizeof (rtn_path);
  const char *s2 = child;

  while (*s != '\0')
    s++;
  while ((s < s_end) && (*s2 != '\0'))
    *s++ = *s2++;
  return (rtn_path);
}
