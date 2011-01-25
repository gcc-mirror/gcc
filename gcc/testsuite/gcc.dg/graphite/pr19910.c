/* Contributed by Volker Reichelt <reichelt@gcc.gnu.org>.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

int a[3];

void foo()
{
  int i, j;

  for (i = 1; i >= 0; --i)
    for (j = i; j >= 0; --j)
      a[i+j] = 0;
}

