/* PR c/71249 */
/* { dg-do compile } */

int
f (int i)
{
  switch (i)
    {
      {
	int j;
      foo:
	return i; /* { dg-bogus "statement will never be executed" } */
      };
    case 3:
      goto foo;
    }
  return i;
}
