/* { dg-do compile } */
/* { dg-options "-O1 -fbranch-probabilities -fno-ipa-pure-const" } */

__attribute__ ((returns_twice)) void
bar (void)
{
}

void
foo (int cond)
{
  if (cond)
    bar ();
} /* { dg-message "profile count data" } */
