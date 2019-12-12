/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

#include <stdio.h>

struct container
{
  int x;
  int y;
};

static struct container max;
static int pole[3][3];
static int pole2[123];

static struct container superpole[10][10];

void f1(struct container *c)
{
  struct container pes;
  pes.x = 123;
  pes.y = 123;

  struct container *pesp = c;
  pesp->x = 5;

  pole[1][2] = 3;

  superpole[4][3].x = 4;
  max.x = 3;
  void *x = &pole;

  int **a = (int**)pole;
  a[1][2] = 543;

  if(x != 0)
    pole[1][2] = 123;
}

void f2(struct container *c)
{
  struct container pes;
  pes.x = 123;
  pes.y = 123;

  struct container *pesp = c;
  pesp->x = 5;

  pole[1][2] = 3;

  superpole[4][3].x = 4;
  max.x = 3;
  void *x = &pole;

  int **a = (int**)pole;
  a[1][2] = 543;

  if(x != 0)
    pole[1][2] = 123;
}

int main(int argc, char **argv)
{
  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:f1->f2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
