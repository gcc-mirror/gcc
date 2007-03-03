// { dg-do compile }
// { dg-options " " }

int f()
{
  if (1)
    {
      return 1;
  else  // { dg-error "expected .\}. before 'else'" }
    {
      return 0;
    }
}
