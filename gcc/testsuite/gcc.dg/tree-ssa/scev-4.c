/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-ivopts" } */

typedef struct {
    int x;
    int y;
} S;

int *a_p;
S a[1000];

void __GIMPLE (startwith ("loop"))
f (int k)
{
  int i;
  int * _1;

bb_2:
  i_5 = k_4(D);
  if (i_5 <= 999)
    goto bb_4;
  else
    goto bb_3;

bb_3:
  return;

bb_4:
  ;

bb_5:
  i_12 = __PHI (bb_6: i_9, bb_4: i_5);
  _1 = &a[i_12].y;
  a_p = _1;
  __MEM <S[1000]> ((int *)&a)[i_12].y = 100;
  i_9 = i_5 + i_12;
  if (i_9 <= 999)
    goto bb_6;
  else
    goto bb_3;

bb_6:
  ;
  goto bb_5;

}

/* { dg-final { scan-tree-dump-times "&a" 1 "ivopts" } } */
