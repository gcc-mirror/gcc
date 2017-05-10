/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

typedef struct
{
  short a;
  short b;
  short c;
} d;
extern d e[];
int f[8];
void
g (d *i)
{
  int h = 0;
  for (; h < 28; h++)
    e[h].a = e[h].b = i[h].a;
  h = 0;
  for (; h < 8; h++)
    f[h] = i[h].b + i[h].c;
  h = 0;
  for (; h < 8; h++)
    f[h] = i[h].b;
}
