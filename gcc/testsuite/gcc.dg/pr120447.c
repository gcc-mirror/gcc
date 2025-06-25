/* { dg-options "-Ofast" } */
/* { dg-additional-options "-mcpu=neoverse-v2" { target aarch64*-*-* } } */

char g;
long h;
typedef struct {
  void *data;
} i;
i* a;
void b(i *j, char *p2);
void c(char *d) {
  d = d ? " and " : " or ";
  b(a, d);
}
void b(i *j, char *p2) {
  h = __builtin_strlen(p2);
  while (g)
    ;
  int *k = j->data;
  char *l = p2, *m = p2 + h;
  l += 4;
  while (l < m)
    *k++ = *l++;
}
