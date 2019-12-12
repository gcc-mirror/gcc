/* PR middle-end/87052 - STRING_CST printing incomplete in Gimple dumps
   { dg-do compile }
   { dg-options "-fdump-tree-original" } */

void* f (char *d, int c)
{
  return __builtin_memchr ("1\0\0", c, 4);
}

/* Veriy the full string appears in the dump:
  { dg-final { scan-tree-dump "\"1\\\\x00\\\\x00\"" "original" } } */
