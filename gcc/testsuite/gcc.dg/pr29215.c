/* PR middle-end/29215 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

char buf[5 * sizeof (int) + 1] __attribute__((aligned (__alignof__ (int))));

static void
foo (int arg1, int arg2, int arg3, int arg4, int arg5)
{
  __builtin_memcpy (buf, &arg1, sizeof (int));
  __builtin_memcpy (buf + sizeof (int), &arg2, sizeof (int));
  __builtin_memcpy (buf + 2 * sizeof (int), &arg3, sizeof (int));
  __builtin_memcpy (buf + 3 * sizeof (int), &arg4, sizeof (int));
  __builtin_memcpy (buf + 4 * sizeof (int), &arg5, sizeof (int));
}

int
main (void)
{
  union { char buf[4]; int i; } u;
  u.i = 0;
  u.buf[0] = 'a';
  u.buf[1] = 'b';
  u.buf[2] = 'c';
  u.buf[3] = 'd';
  foo (u.i, u.i, u.i, u.i, u.i);
  buf[5 * sizeof (int)] = '\0';
  __builtin_puts (buf);
  return 0;
}

/* { dg-final { scan-tree-dump-not "memcpy" "ccp1" } } */
