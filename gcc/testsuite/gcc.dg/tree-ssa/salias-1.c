/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-salias" } */

struct {
  struct {
    struct {
	int i, j;
    } c;
  } b;
} a;

int foo(void)
{
  a.b.c.i = 0;
  return a.b.c.j;
}

/* { dg-final { scan-tree-dump-times "structure field tag SFT" 2 "salias" } } */
/* { dg-final { cleanup-tree-dump "salias" } } */
