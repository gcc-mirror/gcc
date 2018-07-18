/* { dg-do compile } */

int a, b, d, e, f;
int *c;
void g()
{
  for (;;)
    {
      if (*c) {
	  int h;
	  *c = (__UINTPTR_TYPE__) &h;
      } else
	b = 0;
      if (f)
	*c = (__UINTPTR_TYPE__) g;
      else
	for (; a; a++)
	  for (;;) {
	      if (d)
		break;
	      c = (int *) (__UINTPTR_TYPE__) e;
	  }
    }
}
