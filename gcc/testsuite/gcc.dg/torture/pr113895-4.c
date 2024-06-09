/* { dg-do compile } */

long a, b, c;
int d;
long e[2][1];
int f() {
  if (c == a)
    c = b;
}
void g() {
  int h, i = 0;
  for (; f() + d + i; i++)
    e[h][i] = 4;
}
