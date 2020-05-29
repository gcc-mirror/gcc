/* { dg-do compile } */

enum { a = 5, b };
typedef struct {
  int c[b];
} d;
extern d e[];
int f;
int g[6];
void h() {
  int i;
  for (; f; f++) {
    i = 0;
    for (; i < b; i++)
      if (e[f].c[i])
        g[i] = e[f].c[i];
  }
}
