/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-slsr" } */

int __GIMPLE (ssa,startwith("slsr"),guessed_local(1073741824))
main (int argc)
{
  int _1;

  __BB(2,guessed_local(1073741824)):
  if (argc_2(D) == 2)
    goto __BB3(guessed(16777216));
  else
    goto __BB4(guessed(117440512));

  __BB(3,guessed_local(134217728)):
  goto __BB4(precise(134217728));

  __BB(4,guessed_local(1073741824)):
  _1 = __PHI (__BB2: 0, __BB3: 12);
  return _1;
}


/* { dg-final { scan-tree-dump-times "<bb \[0-9\]> \\\[local count: 1073741824" 2 "slsr" } } */
/* { dg-final { scan-tree-dump-times "<bb \[0-9\]> \\\[local count: 134217728" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "goto <bb \[0-9\]>; \\\[12\\\.50%\\\]" 1 "slsr" } } */
/* { dg-final { scan-tree-dump-times "goto <bb \[0-9\]>; \\\[87\\\.50%\\\]" 1 "slsr" } } */
