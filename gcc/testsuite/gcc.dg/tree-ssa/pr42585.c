/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct _fat_ptr
{
  unsigned char *curr;
  unsigned char *base;
  unsigned char *last_plus_one;
};
int Cyc_string_ungetc (int ignore, struct _fat_ptr *sptr);
int
Cyc_string_ungetc (int ignore, struct _fat_ptr *sptr)
{
  struct _fat_ptr *_T0;
  struct _fat_ptr *_T1;
  struct _fat_ptr _T2;
  int _T3;
  struct _fat_ptr _ans;
  int _change;

  {
    _T0 = sptr;
    _T1 = sptr;
    _T2 = *sptr;
    _T3 = -1;
    _ans = _T2;
    _change = -1;
    _ans.curr += 4294967295U;
    *sptr = _ans;
    return (0);
  }
}

/* The local aggregates . */
/* Whether the structs are totally scalarized or not depends on the
   MOVE_RATIO macro defintion in the back end.  The scalarization will
   not take place when using small values for MOVE_RATIO.  */
/* { dg-final { scan-tree-dump-times "struct _fat_ptr _ans" 0 "optimized" { target { ! "powerpc*-*-* arm-*-* sh*-*-* s390*-*-*" } } } } */
/* { dg-final { scan-tree-dump-times "struct _fat_ptr _T2" 0 "optimized" { target { ! "powerpc*-*-* arm-*-* sh*-*-* s390*-*-*" } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
