/* Test that subword atomic operations only affect the subword.  */
/* { dg-do run } */
/* { dg-require-effective-target sync_char_short } */

void
foo (char *x)
{
  __sync_fetch_and_or (x, 0xff);
}

void
bar (short *y)
{
  __atomic_fetch_or (y, 0xffff, 0);
}


int
main ()
{
  char b[4] = {};
  foo(b);

  short h[2] = {};
  bar(h);

  if (b[1] || b[2] || b[3] || h[1])
    __builtin_abort();
}
