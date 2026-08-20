/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -fsanitize=signed-integer-overflow -fdump-tree-ubsan" } */

typedef int v4si __attribute__ ((vector_size (4 * sizeof (int))));

v4si
f (v4si x)
{
  return -(-x);
}

/* { dg-final { scan-tree-dump-times "\\.UBSAN_CHECK_SUB" 2 "ubsan" } } */
