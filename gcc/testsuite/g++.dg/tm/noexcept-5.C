// { dg-do compile { target c++11 } }
// { dg-options "-fgnu-tm -O -fdump-tree-tmmark -fdump-tree-tmlower -Wno-terminate" }

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
/* { dg-final { scan-tree-dump-times "eh_must_not_throw" 2 "tmlower" } } */
/* { dg-final { scan-tree-dump-times "ITM_RU" 1 "tmmark" } } */
