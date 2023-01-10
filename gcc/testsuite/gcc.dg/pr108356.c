/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

char a;
static char c = 3;
char d;
void foo();
short(b)(short e, short f) { return e + f; }
int main() {
  unsigned g = 0;
  if (c)
    ;
  else
    foo();
  for (; g < 2; g = b(g, 2)) {
    d = g ? 0 : a;
    if (g)
      c = 0;
  }
}


/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
