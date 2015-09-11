/* Loop2_invariants pass should distinguish register pressures of different
   register classes.  In this case, register pressue of INT is high.  But
   we can still move the FP invariant out of the loop.  */

/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fira-loop-pressure -fdump-rtl-loop2_invariant " } */

float tt;
extern void foo2 (int *, int *, int *, int *, int *, int *);
extern int foo3 (int, int, int, int, int, int);
int foo (int a, int b, int c, int d)
{
   int i = a;
   int t1, t2, t3, t4, t5, t6;
   t1 = t2 = t3 = t4 = t5 = t6 = 0;

   for (; i > 0; i += c)
    {
      tt += 123456.0;
      if (d > t1 + t2)
	tt = 3.0;
      foo2 (&t1, &t2, &t3, &t4, &t5, &t6);
      t1 += t3 + t4 + a + b;
      t2 -= t5 - t6 - c - d;
    }

 return foo3 (t1, t2, t3, t4, t5, t6);
}

/* { dg-final { scan-rtl-dump "Decided to move invariant" "loop2_invariant"  } } */

