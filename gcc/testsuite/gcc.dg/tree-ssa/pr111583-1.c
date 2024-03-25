/* { dg-do run } */
/* { dg-options "-Os" } */

short a, f, i;
static const int *e;
short *g;
long h;
int main()
{
    {
      unsigned j = i;
      a = 1;
      for (; a; a++) {
	    {
	      __INTPTR_TYPE__ b = j, d = h;
	      int c = 0;
	      while (d--)
		*(char *)b++ = c;
	    }
	  if (e)
	    break;
      }
      j && (*g)--;
      const int **k = &e;
      *k = 0;
    }
  if (f != 0)
    __builtin_abort ();
  return 0;
}
