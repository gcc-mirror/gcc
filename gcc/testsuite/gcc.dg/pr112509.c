/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fno-tree-fre -fno-tree-forwprop" } */

struct S {
  unsigned j : 3;
};
int k, l, m_1 = {0};
void f(int l, struct S x) {
  unsigned int k_1;
  while (m_1 % 8) switch (x.j) {
    case 1:
    case 3:
    case 4:
    case 6:
    case 2:
    case 5: l = m_1;
    case 7:
    case 0: k_1 = 0;
    default: break;
    }
}
void foo(struct S x) { f(l, x); }
