/* This used to abort due to a loop bug on s390*.  */

/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -fPIC" { target s390*-*-* } } */

int count = 0;
char *str;

void test (int flag)
{
  char *p;

  for (;;)
    {
      if (count > 5)
	return;

      p = "test";

      if (flag)
	count++;

      str = p;
    }
}

int main (void)
{
  test (1);

  if (str[0] != 't')
    abort ();

  return 0;
}

