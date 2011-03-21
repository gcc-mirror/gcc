// PR middle-end/45566
// { dg-require-effective-target freorder }
// { dg-options "-O -fnon-call-exceptions -freorder-blocks-and-partition" }

int k;

int
main ()
{
  try
  {
    if (k)
      throw 6;
  }
  catch (...)
  {
  }
}
