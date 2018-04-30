/* { dg-do compile } */
/* { dg-options "-O3 -fgimple -fdump-tree-cunroll-blocks-details" } */

#if __SIZEOF_INT__ < 4
__extension__ typedef __INT32_TYPE__ i32;
#else
typedef int i32;
#endif

struct a {i32 a[8];i32 b;};

void __GIMPLE (startwith("fix_loops"))
t (struct a * a)
{
  i32 i;
  i32 _1;
  i32 _2;
  i32 _9;
  i32 _11;

bb_2:
  _11 = a_6(D)->a[0];
  if (_11 != _Literal (i32) 0)
    goto bb_6;
  else
    goto bb_3;

bb_3:
  return;

bb_4:
  _1 = _2 + 1;
  a_6(D)->a[i_19] = _1;
  i_8 = i_19 + _Literal (i32) 1;
  if (i_8 <= _Literal (i32) 123455)
    goto bb_5;
  else
    goto bb_3;

bb_5:
  i_19 = __PHI (bb_6: _Literal (i32) 1, bb_4: i_8);
  _2 = a_6(D)->a[i_19];
  if (_2 != _Literal (i32) 0)
    goto bb_4;
  else
    goto bb_3;

bb_6:
  _9 = _11 + _Literal (i32) 1;
  a_6(D)->a[0] = _9;
  goto bb_5;
}

/* This testcase relies on the fact that we do not eliminate the redundant test
   for i early.  It is necessary to disable all passes that do so, for the
   moment starting with the loop pipeline is good enough.  */
/* { dg-final { scan-tree-dump-times "Loop 1 iterates 123454 times" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Last iteration exit edge was proved true" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Exit condition of peeled iterations was eliminated" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "loop with 6 iterations completely unrolled" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "cunroll" } } */
