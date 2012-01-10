/* { dg-do compile } */

struct S { int s; } a, *b, **c;
int d;

struct S
foo (void)
{
  struct S s = { 0 }, *e = &a;
  if (d)
    **c = *b;
  while (1)
    {
      *b = s;
      *e = *b;
      if (a.s)
	break;
    }
  return **c;
}
