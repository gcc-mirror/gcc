/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void bar (unsigned int);

int
foo (void)
{
  char buf[1] = { 3 };
  const char *p = buf;
  const char **q = &p;
  unsigned int ch;
  switch (**q)
    {
    case 1:  ch = 5; break;
    default: ch = 0; break;
    }

  bar (ch);
  return ch;
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
