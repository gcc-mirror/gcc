/* { dg-do link } */
/* { dg-options "-O2" } */

extern int link_error (int);

int tst2 (int x, int y)
{
  /* VRP should be able to extract range information for
     x and y out of this TRUTH_AND_EXPR.  */
  if ((x > 5555) && (y < 6666))
    {
      if (x > 5555)
        if (y < 6666)
          return 1111;
        else
          return link_error (2222);
      else
        if (y < 6666)
          return link_error (3333);
        else
          return link_error (4444);
    }
  else
    return 0;
}

int main()
{
  return 0;
}
