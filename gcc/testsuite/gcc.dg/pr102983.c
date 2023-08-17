/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp -fno-tree-ccp" } */
void foo(void);

static int a = 1;

int main() {
  int c = 0;
  for (int b = 0; b <= 0; b++) {
    if (!a)
      foo();
    if (b > c){
      if (c)
        continue;
      a = 0;
    }
    c = 1;
  }
}

/* { dg-final { scan-tree-dump-times "Global Exported: c_.*1, 1" 1 "evrp" } } */
