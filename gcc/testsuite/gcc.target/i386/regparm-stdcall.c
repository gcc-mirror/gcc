/* { dg-do run } */
/* { dg-options -mpreferred-stack-boundary=4 } */
/* { dg-require-effective-target ia32 } */

extern void abort(void);

void __attribute__((regparm(2), stdcall)) foo(int i, int j, float x)
{
  static int last_align = -1;
  int dummy, align = (int)&dummy & 15;
  if (last_align < 0)
    last_align = align;
  else if (align != last_align)
    abort ();
}

int main()
{
	foo(0,0,0.0);
	foo(0,0,0.0);
	return 0;
}
