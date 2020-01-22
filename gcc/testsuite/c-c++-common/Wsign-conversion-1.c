/* PR c++/52703 */
/* { dg-options -Wsign-conversion } */

unsigned f (unsigned x) {
  return x;
}

int main () {
  unsigned short a = 0;
  unsigned b = a + 1;
  f (a + 1);
  return 0;
}
