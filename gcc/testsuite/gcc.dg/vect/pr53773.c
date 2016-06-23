/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-optimized" } */

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

/* { dg-final { scan-tree-dump-times "\\* 10" 2 "optimized" } } */

