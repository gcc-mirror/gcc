/* Basic test of the -mmacosx-version-min option.  */

/* { dg-options "-mmacosx-version-min=10.1" } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}

