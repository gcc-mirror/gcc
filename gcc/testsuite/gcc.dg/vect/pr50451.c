/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int
foo (int integral, int decimal, int power_ten)
{
  while (power_ten > 0)
    {
      integral *= 10;
      decimal *= 10;
      power_ten--;
    }

  return integral+decimal;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

