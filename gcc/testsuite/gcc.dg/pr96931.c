/* { dg-do compile } */
/* { dg-options "-O1 -fpredictive-commoning -fno-tree-loop-im -fdump-tree-pcom-details-blocks" } */

int bl;

void
p3 (void);

void __attribute__ ((returns_twice))
ie (void)
{
  p3 ();

  bl = 0;
  for (;;)
    ++bl;

  ie ();
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "pcom" } } */
