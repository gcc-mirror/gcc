/* { dg-do compile } */
/* { dg-options "-O0 -ftree-ter" } */

long a;
char b;
void e() {
  char f[8][1];
  b = f[a][a];
}

