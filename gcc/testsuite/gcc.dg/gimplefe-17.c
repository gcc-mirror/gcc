/* { dg-do compile } */
/* { dg-options "-fgimple -fdump-tree-fixup_cfg2" } */

int 
__GIMPLE (ssa) *
foo ()
{
  int _1;
  int j;
  int *b;

__BB(5):
  _1 = 1;
  goto __BB2;

__BB(2):
  if (_1)
    goto __BB4;
  else
    goto __BB3;

__BB(3):
  b_2 = (int *)0;
  goto __BB4;

__BB(4):
  b_4 = __PHI (__BB2: b_3(D), __BB3: b_2);
  return b_4;
}

/* { dg-final { scan-tree-dump-not "_1_" "fixup_cfg2" } } */
