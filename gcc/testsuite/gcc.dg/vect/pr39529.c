/* { dg-do compile } */

void
foo (void)
{
  char a[1024];
  char *p = &a[0];
  char *p2;

  p2 = p + 1024;
  do
    {
      p += 2;
      *(p-2) = 1;
      *(p-1) = 1;
    } while (p < p2);
}



