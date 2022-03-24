/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-optimized --param=gimple-fe-computed-hot-bb-threshold=10 " } */

int __GIMPLE (ssa,startwith("slsr"),precise(3))
main (int argc)
{
  int _1;

  __BB(2,precise(3)):
  if (argc_2(D) == 2)
    goto __BB3(precise(44739243));
  else
    goto __BB4(precise(89478485));

  __BB(3,precise(1)):
  goto __BB4(precise(134217728));

  __BB(4,precise(3)):
  _1 = __PHI (__BB2: 0, __BB3: 12);
  return _1;
}


/* { dg-final { scan-tree-dump-times "<bb \[0-9\]> \\\[count: 3" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "<bb \[0-9\]> \\\[count: \[12\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "goto <bb \[0-9\]>; \\\[33\\\.33%\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "goto <bb \[0-9\]>; \\\[66\\\.67%\\\]" 1 "optimized" } } */
