/* { dg-additional-options "-O1" } */
unsigned b = 0xce8e5a48, c = 0xb849691a;
unsigned a[8080];
int main() {
  a[0] = b;
  c = c;
  unsigned f = 0xb1e8;
  for (int h = 0; h < 5; h++)
    a[h] = (b & c) ^ f;
  if (a[0] != 0x8808f9e0)
    __builtin_abort();
}
