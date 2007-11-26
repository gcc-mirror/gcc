/* PR 23722 */
/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */
int f()
{
  if (1)
    {
      return 1;
    }
  else 
    {
      else; /* { dg-error "'else' without a previous 'if'" } */
    }
}
