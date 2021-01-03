/* { dg-do run } */
/* { dg-additional-options "-fdisable-tree-evrp" } */

unsigned char c;

int main() {
volatile short b = 4066;
  unsigned short bp = b;
  unsigned d = bp & 2305;
  signed char e = d;
  c = e ? : e;
  if (!d)
    __builtin_abort ();
  return 0;
}
