/* PR 23722 */
/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */
int f()
{

  else  /* { dg-error "'else' without a previous 'if'" } */
    {
      return 0;
    }
}
