/* { dg-do run } */
/* { dg-options "-O3 -fno-early-inlining -fno-tree-fre -fno-tree-pre -fno-code-hoisting -fno-ipa-cp" } */

struct a {
  int b;
} const c;
void d(char p, struct a e) {
  while (e.b)
    ;
}
static unsigned short f(const struct a g) {
  d(g.b, g);
  return g.b;
}
int main() {
  return f(c);
}
