/* Ensure that bogus clauses aren't propagated in combined loop
   constructs.  */

int
main ()
{
  int a, i;

#pragma acc parallel loop vector copy(a[0:100]) reduction(+:a) /* { dg-error "'a' does not have pointer or array type" } */
  for (i = 0; i < 100; i++)
    a++;


#pragma acc serial loop vector copy(a[0:100]) reduction(+:a) /* { dg-error "'a' does not have pointer or array type" } */
  for (i = 0; i < 100; i++)
    a++;


  return a;
}
