/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mpreferred-stack-boundary=3 -mapx-features=push2pop2 -fomit-frame-pointer" } */

extern int bar (int);

void foo ()
{
  int a,b,c,d,e,f,i;
  a = bar (5);
  b = bar (a);
  c = bar (b);
  d = bar (c);
  e = bar (d);
  f = bar (e);
  for (i = 1; i < 10; i++)
  {
    a += bar (a + i) + bar (b + i) +
         bar (c + i) + bar (d + i) +
         bar (e + i) + bar (f + i);
  }
}

/* { dg-final { scan-assembler-not "push2(|p)\[\\t \]*%r" } } */
/* { dg-final { scan-assembler-not "pop2(|p)\[\\t \]*%r" } } */
