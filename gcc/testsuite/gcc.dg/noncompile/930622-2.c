f ()
{
  int i;
  for (i--)	/* { dg-error "parse|syntax|expected" } */
    ;
}
