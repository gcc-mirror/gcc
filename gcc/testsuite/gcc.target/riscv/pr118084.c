/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32izk -mabi=ilp32 -Os" } */
unsigned a;
int main() {
  int b = 8;
  for (; b; b--)
    if (a & 1)
      a = a >> 1 ^ 30196000;
    else
      a >>= 1;
}


