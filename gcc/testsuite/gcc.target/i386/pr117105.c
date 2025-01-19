/* { dg-do compile } */
/* { dg-options "-O3 -fno-code-hoisting -fno-tree-fre -fno-tree-dominator-opts -fno-tree-pre -fno-tree-sra" } */
int a;
struct b {
  char c;
  char d;
};
int main() {
  struct b e;
  int f;
  while (a)
    if (f == e.d)
      f = e.c = e.d & 1 >> e.d;
  return 0;
}
