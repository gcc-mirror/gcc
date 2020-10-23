/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre" } */

char a;
void b() {
  char *c[5];
  char *d = &a;
  &d;
  *(c[4] = d);
}
int main() { return 0; }
