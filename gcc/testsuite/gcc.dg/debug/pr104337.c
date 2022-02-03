/* { dg-do compile } */

struct a {
  unsigned b : 7;
};
inline __attribute__((optimize(3))) __attribute__((always_inline)) struct a
c() {
  struct a d;
  return d;
}
void e() {
  for (;;)
    c();
}
int main() {}
