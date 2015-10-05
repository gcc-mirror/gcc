/* { dg-do compile } */

int isdigit (int);

int
foo (const char *s)
{
  int success = 1;
  const char *p = s + 2;
  if (!isdigit (*p))
    success = 0;
  while (isdigit (*p))
    ++p;
  return success;
}
