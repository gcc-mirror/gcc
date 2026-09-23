/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int h, q, r;
int o[27];

__attribute__((noipa))
void z (int r) {
  int q = -13363 + r; // 27
  int p = -71005931 * q + ~51083117 + 1968243255; // == 0
  while (p != 12)
  {
    o[p] = 1 + p;
    p = 1 + p;
  }
}

int main () {
  z(13390);
}
