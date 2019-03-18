/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-ivopts" } */

int *a_p;
int a[1000];

void __GIMPLE (ssa,startwith ("loop"))
f (int k)
{
  long long int i;
  int * _1;

__BB(2):
  i_5 = (long long int) k_4(D);
  if (i_5 <= 999ll)
    goto __BB4;
  else
    goto __BB3;

__BB(3):
  return;

__BB(4):
  goto __BB5;

__BB(5):
  i_12 = __PHI (__BB6: i_9, __BB4: i_5);
  _1 = &a[i_12];
  a_p = _1;
  __MEM <int[1000]> ((int *)&a)[i_12] = 100;
  i_9 = i_5 + i_12;
  if (i_9 <= 999ll)
    goto __BB6;
  else
    goto __BB3;

__BB(6):
  ;
  goto __BB5;

}

/* { dg-final { scan-tree-dump-times "&a" 1 "ivopts" } } */
