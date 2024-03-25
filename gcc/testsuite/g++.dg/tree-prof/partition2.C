// PR middle-end/45458
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-require-effective-target freorder }
// { dg-options "-O2 -fnon-call-exceptions -freorder-blocks-and-partition" }

int
main ()
{
  try
  {
    throw 6;
  }
  catch (...)
  {
  }
}
