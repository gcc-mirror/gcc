/* { dg-do compile } */
/* { dg-additional-options "-g" } */

extern int foo(void);

int main(void)
{
  int i, a, b;

  if (foo())
    return 0;

  for (i = 0, a = 0, b = 0; i < 3; i++, a++)
  {
    if (foo())
      break;

    if (b += a)
      a = 0;
  }

  if (!a)
    return 2;

  b += a;

  return 0;
}
