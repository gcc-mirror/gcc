/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8m_base_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8m_base } */

long long a;
int b, c;
int d(int e, int f) { return e << f; }
void g() {
  long long h;
  char i = d(b >= 7, 2);
  c = i == 0 ?: 1 / i;
  h = c && a ?: c + a;
  b = h;
}
