/* { dg-do compile } */

int a, b, c;

void d()
{
  int e, f;
  for (; a; a++)
    {
      e = (__UINTPTR_TYPE__)d;
      b = 0;
      for (; b < 2; b++)
	{
	  f = e = e / 2;
	  c = c + f;
	}
    }
}
