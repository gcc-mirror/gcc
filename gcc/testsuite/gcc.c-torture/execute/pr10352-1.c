/* this is another case where phiopt
   would create -signed1bit which is undefined. */
struct {
  int a:1;
} b;
int *c = (int *)&b, d;
int main() {
  d = c && (b.a = (d < 0) ^ 3);
  if (d != 1)
    __builtin_abort();
  return 0;
}
