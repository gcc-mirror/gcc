/* { dg-do run { target x86_64-*-* i?86-*-* } } */
/* { dg-require-effective-target lp64 } */

unsigned a[4] = {1,1,1,1};
unsigned tt1 = 0;

__attribute__((noipa))
static void bug(unsigned * p, unsigned *t, int n, int t2)
{
  for(int i = 0; i < n; i++)
    {
      _Bool LookupFlags ;
      unsigned v = t[i];
      unsigned tt = tt1;
      if (v == 0)
	LookupFlags = 0;
      else if (v == 1)
	LookupFlags = 1;
      if (LookupFlags) {
	  tt|=3u;
	  LookupFlags = 0;
      }
      asm("movq $-1, %q1":"+a"(LookupFlags));
      *p = tt;
    }
}

int main()
{
  unsigned r = 42;
  bug(&r,a, sizeof(a)/sizeof(a[0]), 1);
  __builtin_printf("%u\n", r);
  if (r != 3) __builtin_abort();
}
