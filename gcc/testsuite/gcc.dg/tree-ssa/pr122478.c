/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-additional-options "-fgimple" } */

unsigned char __GIMPLE (ssa)
foo (unsigned short mask__701)
{
  _Bool _19;
  unsigned char _180;

__BB(2):
  _19 = __BIT_FIELD_REF <_Bool> (mask__701, 1, 12);
  _180 = __VIEW_CONVERT<unsigned char>(_19);
  return _180;
}

/* { dg-final { scan-tree-dump-times "VIEW_CONVERT_EXPR" 1 "optimized" } } */
