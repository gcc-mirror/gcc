/* { dg-do compile } */
/* { dg-additional-options "-O2 -fdump-ipa-icf-optimized" } */

/* Picking 'main' as a candiate target for equivalent functios is not a
   good idea.  */

int baz (int);

int foo ()
{
  return baz (baz (0));
}


int main ()
{
  return baz (baz (0));
}

/* Notice the two functions are the same.  */
/* { dg-final { scan-ipa-dump "Semantic equality hit:foo/\[0-9+\]+->main/\[0-9+\]+" "icf" } } */

/* Make sure we don't tail call main.  */
/* { dg-final { scan-ipa-dump-not "= main \\(\\);" "icf" } } */

/* Make sure we tail call foo.  */
/* { dg-final { scan-ipa-dump "= foo \\(\\);" "icf" } } */
