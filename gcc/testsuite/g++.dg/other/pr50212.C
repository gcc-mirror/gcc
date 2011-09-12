// PR rtl-optimization/50212
// { dg-do compile }
// { dg-require-effective-target freorder }
// { dg-options "-O -fnon-call-exceptions -ftrapv -freorder-blocks-and-partition" }

void
foo (int n)
{
  try
  {
    int i = 0;
    while (i++ < n);
  }
  catch (...)
  {
  }
}
