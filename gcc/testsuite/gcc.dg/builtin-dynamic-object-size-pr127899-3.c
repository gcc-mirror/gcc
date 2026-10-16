/* { dg-do compile } */
/* { dg-options "-O2" } */

__SIZE_TYPE__ h (int n)
{
  char *q = __builtin_malloc (n);
  return __builtin_dynamic_object_size (q - 8, 2);
}

extern char a[10], b[20];
__SIZE_TYPE__ k (int c)
{
  char *q = c ? a : b;
  return __builtin_dynamic_object_size (q - 8, 2);
}
