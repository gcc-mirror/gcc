/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

int a;
int *b = &a;
int **c = &b;
int d;
void bar25_(void);
void foo(void);
int main() {
  int __attribute__((aligned(sizeof(int)))) e[][1] = {0, 0, 0, 0, 0, 1};
  for (;;) {
    bar25_();
    /* We should optimistically treat a == 0 because of the bounds of
       the object.  */
    if (e[5][a])
      break;
    e[a][0] = 0;
    foo();
  }
  *c = &d;
}

/* { dg-final { scan-tree-dump-not "foo" "fre1" { xfail *-*-* } } } */
