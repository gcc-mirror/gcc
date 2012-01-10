// { dg-do compile }
// { dg-options "-fgnu-tm -O -std=c++0x -fdump-tree-tmmark -fdump-tree-tmlower" }

int global;

void f2(int x)
{
  __transaction_atomic
    {
      __transaction_atomic noexcept(true)
        {
	  global += 1;
	  if (x)
	      throw 23;
        }
    }
}
/* { dg-final { scan-tree-dump-times "eh_must_not_throw" 1 "tmlower" } } */
/* { dg-final { scan-tree-dump-times "ITM_RU" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmlower" } } */
