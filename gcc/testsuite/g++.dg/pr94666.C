// { dg-do compile }
// { dg-options "-O3" }
// { dg-additional-options "-march=z13" { target s390*-*-* } }

int a, c;
struct A {
  int e() {
    int f;
    for (int b = 0; b < 4; b++) {
      a = __builtin_popcountl(d[b]);
      f += a;
    }
    return f;
  }
  long d[4];
} * g;
void h() {
  for (int b; b; b++)
    c += g[b].e();
}
