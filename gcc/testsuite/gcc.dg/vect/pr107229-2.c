/* { dg-do compile } */
/* PR tree-optimization/107229.  */

int a, c;
struct {
  long f;
  long g;
  long d;
  int : 8;
  int : 27;
  int e : 21;
} f;
void g(int b) { a = a & 1; }
int main() {
  while (c)
    g(f.e);
  return 0;
}
