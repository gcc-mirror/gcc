int f(int c)
{
  switch (c)
  {
    case d: /* { dg-error "'d' was not declared" } */
     int optBzip2 = true;
  }
}
