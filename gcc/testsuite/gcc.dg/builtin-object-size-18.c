/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* __stpncpy_chk could return buf up to buf + 64, so
   the minimum object size might be far smaller than 64.  */
/* { dg-final { scan-tree-dump-not "return 64;" "optimized" } } */

typedef __SIZE_TYPE__ size_t;

size_t
foo (const char *p, size_t s, size_t t)
{
  char buf[64];
  char *q = __builtin___stpncpy_chk (buf, p, s, t);
  return __builtin_object_size (q, 2);
}
