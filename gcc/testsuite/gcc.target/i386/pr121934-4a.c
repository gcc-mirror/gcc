/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-dominator-opts -fno-tree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-pre -fno-tree-fre -mno-avx512f -mavx -mprefer-vector-width=256" } */

extern int f();
int a, b, c;
_BitInt(256) d[3];
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
      d[h] = (_BitInt(256)) -1;
    }
    goto i;
  }
}

/* { dg-final { scan-assembler-not "rep stos" } } */
