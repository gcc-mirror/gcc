/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-dominator-opts -fno-tree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-pre -fno-tree-fre" } */

extern int f();
int a, b, c;
long long int d[3];
void g() {
  int h;
  if (f()) {
    if (b)
    i:
      c > 0;
    a = 0;
    for (h = 0; h < 3; h++) {
      if (a != 1)
        __builtin_printf("0\n");
      d[h] = (long long int) -1;
    }
    goto i;
  }
}

/* { dg-final { scan-assembler-not "rep stos" } } */
