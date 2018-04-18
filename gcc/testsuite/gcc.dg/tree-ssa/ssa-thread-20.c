/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-ethread-stats" } */

/* Test that we thread 2 -> 3 -> 5 in the early threader.  */

int no;
int yes;

void __GIMPLE (startwith ("ethread")) foo (int x)
{
 bb_2:
  if (x == 222)
    goto bb_3;
  else
    goto bb_5;

 bb_3:
  if (x == 444)
    goto bb_4;
  else
    goto bb_5;

 bb_4:
  no = 123;

 bb_5:
  yes = 456;
}

/* { dg-final { scan-tree-dump-times "Jumps threaded: 1" 1 "ethread" } } */
