/* Verify that logic combining probabilities works as expected.  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline -fno-early-inlining"  } */

struct bah {int a,b,d;};
void test3 (int, int, int, int, int);

__attribute__ ((noinline))
void test(int a,int b,int c,int d,int e)
{
  test3(a,b,c,d,e);
}
inline
static void bar (int parm1, int parm2)
{
  int i;
  for (i = 0; i<10; i++)
    {
      test (0,0,parm1,parm2,i);
    }
}
void foo (int invariant)
{
  int i;
  for (i = 0; i<10; i++)
    {
      bar (i, invariant);
    }
}
/* After inlining bar into foo, op2 is invariant within inner loop.  */
/* { dg-final { scan-ipa-dump "op2 change 9.990000. of time"  "inline"  } } */
/* After inlining bar into foo, op3 is invariant within both loops.  */
/* { dg-final { scan-ipa-dump "op3 change 1.000000. of time"  "inline"  } } */
