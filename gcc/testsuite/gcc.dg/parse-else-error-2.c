/* PR 23722 */
/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */
int f()
{
  if (1)
    {
      return 1;
  else /* { dg-error "expected .\}. before 'else'" } */
  {
      }
  }
} /* { dg-error "expected identifier or '\\(' before .\}. token" } */
