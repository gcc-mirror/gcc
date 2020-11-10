/* { dg-do compile } */
/* { dg-options "-Wall -Wextra -fno-strict-aliasing -fwrapv -Os -fno-toplevel-reorder -fno-tree-ccp -fno-tree-fre" } */

short a = 0;
long b = 0;
char c = 0;
void d() {
  int e = 0;
f:
  for (a = 6; a;)
    c = e;
  e = 0;
  for (; e == 20; ++e)
    for (; b;)
      goto f;
}
int main() { return 0; }
