/* { dg-do compile } */
/* { dg-options "-O -ftree-parallelize-loops=2 -ftree-vectorize -fno-tree-ch -fno-tree-dominator-opts" } */

void abort (void);

int d[1024], e[1024];

int
foo (void)
{
  int s = 0;
  int i;

  for (i = 0; i < 1024; i++)
    s += d[i] - e[i];

  return s;
}

int
main ()
{
  int i;

  for (i = 0; i < 1024; i++)
    {
      d[i] = i * 2;
      e[i] = i;
    }

  if (foo () != 1023 * 1024 / 2)
    abort ();

  return 0;
}
