/* { dg-do compile } */
/* { dg-options "-Os -fcompare-debug" } */

int a, b, c;
void fn1 ()
{
  int d;
  if (fn2 () && !0)
    {
      b = (
	   {
	   int e;
	   fn3 ();
	   switch (0)
	   default:
	   asm volatile("" : "=a"(e) : "0"(a), "i"(0));
	   e;
	   });
      d = b;
    }
  c = d;
}
