/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* PR tree-optimization/110941 */
/* The call to foo should be able to removed,
   VRP should figure out `(c >= 2 && c <= 26)`
   is always true.  */  

static int a;
void foo(void);
void bar349_(void);
void bar363_(void);
void bar275_(void);
int main() {
  {
    {
      short b = 26;
      for (; b >= 1; b = b - 4) {
        if (b >= 2 && b <= 26)
          bar275_();
        if (a)
          bar363_();
        if (a)
          bar349_();
        int c = b;
        if (!(c >= 2 && c <= 26))
          foo();
      }
    }
    a = 0;
  }
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
