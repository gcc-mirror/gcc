/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-vrp -fno-inline" } */

struct a {
  int b;
};
int c;
signed char d;
static int e(long long f) { return f < 0; }
static void g(unsigned f) { c = e(~f); }
int main() {
  int h;
  struct a i = {128};
  h = d > i.b;
  g(h);
  if (h)
    __builtin_abort();
  if (c)
    __builtin_abort();
  return 0;
}
