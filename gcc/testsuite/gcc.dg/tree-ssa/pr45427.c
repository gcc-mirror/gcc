/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details" } */

extern void abort (void);
int __attribute__((noinline,noclone))
foo (char *p)
{
  int h = 0;
  do
    {
      if (*p == '\0')
        break;
      ++h;
      if (p == 0)
        abort ();
      ++p;
    }
  while (1);
  return h;
}
int main()
{
  if (foo("a") != 1)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "bounded by 0" 0 "cunrolli"} } */
/* { dg-final { cleanup-tree-dump "cunrolli" } } */
