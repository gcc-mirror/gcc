/* { dg-do compile } */
int a;
void b() {
  int c;
  unsigned d = c + 19;
  a = d >> 32 + 19 + d + 255 - 293;
}
