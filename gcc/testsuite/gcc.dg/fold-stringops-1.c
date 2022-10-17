/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lower-details" } */

char dst[2048];

char *
copy1 (const char *src, int cond)
{
  __builtin___stpncpy_chk (dst, src, 42, __builtin_object_size (dst, 0));

  return dst;
}

char *
copy2 (void)
{
  __builtin___stpcpy_chk (dst, "Hello world", __builtin_object_size (dst, 0));

  return dst;
}
/* { dg-final { scan-tree-dump "simplified __builtin___stpncpy_chk to __builtin_strncpy" "lower" } } */
/* { dg-final { scan-tree-dump "simplified __builtin___stpcpy_chk to __builtin_strcpy" "lower" } } */

