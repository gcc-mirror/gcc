/* { dg-do compile { target { arm_thumb2_ok } } } */
/* { dg-options "-O2 -fdump-rtl-loop2_invariant " } */

int t1, t2, t3, t4, t5, t6, t7, t8, t9, t10;
extern void foo2 (int *, int *, int *, int *, int *, int *);
extern int foo3 (int, int, int, int, int, int);
int foo (int a, int b, int c, int d)
{
   int i = a;

   for (; i > 0; i += b)
    {
      if (a > 0x1234567)
	foo2 (&t1, &t2, &t3, &t4, &t5, &t6);
      foo2 (&t1, &t2, &t3, &t4, &t5, &t6);
      if (b > 0x1234567)
	foo2 (&t7, &t2, &t8, &t4, &t5, &t6);
      foo2 (&t1, &t2, &t3, &t4, &t5, &t6);
      if (c > 0x1234567)
	foo2 (&t1, &t9, &t10, &t4, &t5, &t6);
      t2 = t5 - d;
    }

 return foo3 (t1, t2, t3, t4, t5, t6);
}

/* { dg-final { scan-rtl-dump "Decided to move invariant 0" "loop2_invariant" } } */

