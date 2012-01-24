/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmedge" } */
__thread int notshared = 0;
int shared = 0;

int main()
{
  __transaction_atomic
    {
      notshared++;
      shared++;
    }
  return notshared + shared;
}
/* { dg-final { scan-tree-dump-times "tm_save.\[0-9_\]+ = notshared" 1 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "notshared = tm_save" 1 "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
