/* { dg-do compile } */
/* PR tree-optimization/123745 */

struct b {
  unsigned long c;
  unsigned long d;
};
void l(struct b *);
typedef int v4i __attribute__((vector_size(4*sizeof(int))));
v4i t;
v4i t1;
void m(int n) {
  struct b o;
  v4i t2 = t;
  t1 = t2;
  o.d = 1LU << n;
  unsigned long g = t2[0];
  o.c = g << n;
  l(&o);
}
