/* { dg-do compile } */

short d, e;
int f;
extern short g[][24];
char c;
void h() {
  char a = 6;
  c = a;
  for (unsigned long a = (d || e) - 1; a < c; a += f)
    for (signed b = 0; b < 24; b++)
      g[a][b] = 4;
}
