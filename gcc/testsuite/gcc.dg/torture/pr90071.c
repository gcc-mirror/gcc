/* { dg-do compile } */

int a;
static int b;

void
foo ()
{
  int d;
  int e = (int) (__INTPTR_TYPE__) &&f;
  void *g = &&h;
h: ++e;
   if (a)
     i: goto *g;
   for (;;)
     {
       e = 0;
       if (b)
	 goto i;
     }
f:
   goto *({ d || e < 0 || e >= 2; });
   &e;
}
