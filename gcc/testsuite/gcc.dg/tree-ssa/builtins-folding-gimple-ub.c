/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

char *buffer1;
char *buffer2;

#define SIZE 1000

int
main (void)
{
  const char* const foo1 = "hello world";

  /* MEMCHR.  */
  if (__builtin_memchr ("", 'x', 1000)) /* Not folded away.  */
    __builtin_abort ();
  if (__builtin_memchr (foo1, 'x', 1000)) /* Not folded away.  */
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "__builtin_memchr" 2 "optimized" } } */
