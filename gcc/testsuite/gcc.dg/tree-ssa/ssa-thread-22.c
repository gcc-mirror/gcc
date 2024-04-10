/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */
/* PR tree-optimization/107823 */
/* With jump threading across the loop header,
   we should figure out that b is always 0 and remove
   the call to foo.  */

int a;
void bar64_(void);
void foo();
int main() {
  signed char b = a = 6;
  for (; a; a = 0) {
    bar64_();
    b = 0;
  }
  if (b <= 0)
    ;
  else
    foo();
}

/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
