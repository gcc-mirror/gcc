/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

int b;
void foo();
void(a)();
int main() {
  int c;
  int *d = &c;
  *d = a && 8;
  b = 0;
  for (; b < 9; ++b)
    *d ^= 3;
  if (*d)
    ;
  else
    foo();
}

/* { dg-final { scan-tree-dump-not "foo" "evrp" } } */
