/* { dg-do compile } */
/* { dg-options "-O2" } */
void __assert_fail();
struct a {
  int b;
  int c;
  int d;
  int : 2;
};
int e, f;
struct a g, i;
const struct a h;
int main() {
  struct a j;
  g = h;
  if (e)
    __assert_fail();
  if (f)
    j = h;
  i = j;
  return 0;
}
