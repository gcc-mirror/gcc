/* { dg-lto-do link } */
/* { dg-require-linker-plugin "" } */
/* { dg-lto-options { { -O2 -flto -fdump-tree-optimized } } } */


int callee_ptr(const char *p);
int main ()
{
  char s[16];
  return callee_ptr(s);
}

/* { dg-final { scan-ltrans-tree-dump-not "return 0;" "optimized" } } */
