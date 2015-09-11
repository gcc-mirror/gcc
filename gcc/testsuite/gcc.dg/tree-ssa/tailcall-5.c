/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc" } */

void
set_integer (void *dest, int value, int length)
{
  int tmp = value;
  __builtin_memcpy (dest, (void *) &tmp, length);
}

/* { dg-final { scan-tree-dump-not "tail call" "tailc" } } */
