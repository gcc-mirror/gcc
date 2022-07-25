/* { dg-do run } */

int a, c, e;
const int b = 1;
char d;
int main() {
  a = ~(e || 0) ^ b & ~d;
  d = ~(a | ~2);
  if (d)
    __builtin_abort();
  return 0;
}
