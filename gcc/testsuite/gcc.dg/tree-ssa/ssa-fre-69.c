/* { dg-do compile } */
/* { dg-options "-fgimple -O1 -fdump-tree-fre1" } */

int a[10];
typedef int v2si __attribute__((vector_size(__SIZEOF_INT__*2)));
int __GIMPLE (ssa,guessed_local(97603132),startwith("fre1"))
     foo ()
{
  int i;
  int _59;
  int _44;
  int _13;
  int _18;
  v2si _80;
  v2si _81;
  int res;

  __BB(2,guessed_local(97603132)):
  _59 = 64;
  i_61 = 9;
  _44 = i_61 * i_61;
  _80 = _Literal (v2si) {_59, _44};
  _81 = _80;
  __MEM <v2si> ((int *)&a + _Literal (int *) 32) = _81;
  i_48 = 9;
  _13 = a[8];
  _18 = a[i_48];
  res_15 = _13 + _18;
  return res_15;
}

/* { dg-final { scan-tree-dump "return 145;" "fre1" } } */
