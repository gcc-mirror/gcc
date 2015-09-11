/* PR tree-optimization/64715 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern inline __attribute__ ((always_inline, gnu_inline, artificial, nothrow, leaf)) char *
strcpy (char *__restrict dest, const char *__restrict src)
{
  return __builtin___strcpy_chk (dest, src, __builtin_object_size (dest, 2 > 1));
}

const char *str1 = "JIHGFEDCBA";
void bar (char *);

void
foo ()
{
  struct A { char buf1[9]; char buf2[1]; } a;
  strcpy (a.buf1 + (0 + 4), str1 + 5);
  bar ((char *) &a);
}

/* { dg-final { scan-tree-dump "__builtin___strcpy_chk\[^;\n\r\]*, 5\\\);" "optimized" } } */
