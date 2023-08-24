/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-do compile } */
/* Disable BB vectorization, we now can cover the whole loop with that
   when the target supports .REDUC_PLUS for integers, we don't have a
   good target selector for this.  */
/* { dg-additional-options "-fdump-tree-optimized -fno-tree-slp-vectorize" } */

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

/* We can avoid a scalar tail when using fully-masked loops with a fixed
   vector length.  */
/* { dg-final { scan-tree-dump-times "\\* 10" 2 "optimized" { target { { ! vect_fully_masked } || vect_variable_length } } } } */
/* { dg-final { scan-tree-dump-times "\\* 10" 0 "optimized" { target { vect_fully_masked && { ! vect_variable_length } } } } } */

