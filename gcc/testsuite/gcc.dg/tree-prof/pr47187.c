/* PR bootstrap/47187 */
/* { dg-options "-O2" } */

char buf[64];
char buf2[64];

void *
foo (char *p, long size)
{
  return __builtin_memcpy (buf, p, size);
}

int
main (void)
{
  long i;
  for (i = 0; i < 65536; i++)
    if (foo ("abcdefghijkl", 12) != buf)
      __builtin_abort ();
  if (foo (buf2, 64) != buf)
    __builtin_abort ();
  return 0;
}
