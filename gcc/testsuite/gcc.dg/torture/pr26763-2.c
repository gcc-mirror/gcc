/* { dg-do run } */

extern void abort(void);

int try (char *a, __SIZE_TYPE__ d)
{
  return a + d > a;
}

int main(void)
{
  char bla[100];

  if (try (bla + 50, -1))
    abort ();

  return 0;
}
