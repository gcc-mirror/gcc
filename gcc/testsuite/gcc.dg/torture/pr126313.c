/* { dg-do run } */
/* PR tree-optimization/126313 */
unsigned char a = 3;
short b, c, d;
int e;
static inline
char(f)(signed char g, signed char p2) {
  return p2 == 0 || g && p2 == 1 ? 0 : g % p2; 
}
static inline
unsigned i(short g) {
  d = g;
  return c;
}
static inline
void fn3(signed char g, int p2) {
  if (!(1 >= p2 && p2 <= g))
    e = b | i(f(1, p2) > 0xE151060F);
  else {
    { d = p2; }
  }
}
int main() {
  fn3(a, a);
  if (d != 0)
    __builtin_abort ();
}
