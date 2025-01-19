/* { dg-do compile } */
/* { dg-options "-O3 -fgimple -fdump-tree-cunroll-blocks-details -fno-tree-vectorize" } */

#if __SIZEOF_INT__ < 4
__extension__ typedef __INT32_TYPE__ i32;
#else
typedef int i32;
#endif

struct a {i32 a[8];i32 b;};

void __GIMPLE (ssa,startwith("fix_loops"))
t (struct a * a)
{
  i32 i;
  i32 _1;
  i32 _2;
  i32 _9;
  i32 _11;

__BB(2):
  _11 = a_6(D)->a[0];
  if (_11 != _Literal (i32) 0)
    goto __BB6;
  else
    goto __BB3;

__BB(3):
  return;

__BB(4):
  _1 = _2 + _Literal (i32) 1;
  a_6(D)->a[i_19] = _1;
  i_8 = i_19 + _Literal (i32) 1;
  if (i_8 <= _Literal (i32) 123455)
    goto __BB5;
  else
    goto __BB3;

__BB(5):
  i_19 = __PHI (__BB6: _Literal (i32) 1, __BB4: i_8);
  _2 = a_6(D)->a[i_19];
  if (_2 != _Literal (i32) 0)
    goto __BB4;
  else
    goto __BB3;

__BB(6):
  _9 = _11 + _Literal (i32) 1;
  a_6(D)->a[0] = _9;
  goto __BB5;
}

/* This testcase relies on the fact that we do not eliminate the redundant test
   for i early.  It is necessary to disable all passes that do so, for the
   moment starting with the loop pipeline is good enough.  */
/* { dg-final { scan-tree-dump-times "Loop 1 iterates 123454 times" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Last iteration exit edge was proved true" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "Exit condition of peeled iterations was eliminated" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-times "loop with 6 iterations completely unrolled" 1 "cunroll" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "cunroll" } } */
