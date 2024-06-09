/* Verify we don't ICE on certain float conditionals.  */

/* { dg-additional-options "-Ofast" } */

int test_pr111881 (float sf1)
{
  return sf1 <= 0 || sf1 >= 7 ? 0 : sf1;
}
