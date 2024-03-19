/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

unsigned short a = 65535;
int b, f, g;
int *c = &b;
long d;
short e;
static int *h(int);
void i() { h(a); }
int *h(int j) {
  unsigned char k;
  for (; e != 8; e = e + 4)
    k = 0;
  for (; (unsigned char)(j-181249535) + k <= 1; k++) {
    *c = d;
    for (; f; f++)
      ;
  }
  return &g;
}
int main() { i(); }
