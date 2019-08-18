/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O -fgimple -fdump-tree-fre1" } */

typedef int v4si __attribute__((vector_size(__SIZEOF_INT__ * 4)));
#if __SIZEOF_INT__ == 4
__GIMPLE (ssa) int foo (int *a)
{
  v4si _2;
  int _3;
  int _4;
  int _5;
  int _6;
  int _7;
  int _8;
  int _9;

__BB(2):
  __MEM <unsigned char[3 * __SIZEOF_INT__]> ((char *)a_1(D) + 4) = _Literal (unsigned char[3 * __SIZEOF_INT__]) {};
  __MEM <int> (a_1(D) + 8) = 2;
  __MEM <int> (a_1(D)) = 1;
  _2 = __MEM <v4si> (a_1(D));
  _3 = __BIT_FIELD_REF <int> (_2, 32, 0);
  _4 = __BIT_FIELD_REF <int> (_2, 32, 32);
  _5 = __BIT_FIELD_REF <int> (_2, 32, 64);
  _6 = __BIT_FIELD_REF <int> (_2, 32, 96);
  _7 = _3 + _4;
  _8 = _7 + _5;
  _9 = _8 + _6;
  return _9;
}
#endif

/* { dg-final { scan-tree-dump "return 3;" "fre1" } } */
