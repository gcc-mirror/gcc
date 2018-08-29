/* { dg-do compile } */

struct a
{
  unsigned long b;
  unsigned long c;
  int d;
  int *e;
  char f;
};

struct
{
  int g;
  struct a h[];
} i;

int j, k;
void l ()
{
  for (; k; k++)
    j += (int) (i.h[k].c - i.h[k].b);
}
