/* { dg-do compile } */

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

/* Two occurrences in annotations, two in code.  */
/* { dg-final { scan-tree-dump-times "\\* 10" 4 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

