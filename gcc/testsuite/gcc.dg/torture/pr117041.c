/* { dg-do compile } */

unsigned short a;
int b, c[7][6];
int main() {
  for (a = 0; a < 6; a++)
    for (b = 5; b; b--)
      c[a][b] = c[a+1][b];
  return 0;
}
