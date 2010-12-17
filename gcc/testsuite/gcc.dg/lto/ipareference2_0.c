/* { dg-lto-options {{ -O1 -flto -flto-partition=1to1 -fwhole-program}} } */
/* { dg-lto-do run } */

/* Verify that ipa-reference marks A as constant and we fold references
   to a[1] and a[2] to &c and that we promote C to hidden vars shared across ltrans units. */
void abort (void);
int b,c,d,e,f;
int *a[5]={&b,&c,&c,&e};
void other_ltrans (void);
main()
{
  other_ltrans ();
  if (*(a[1])!=11)
    abort ();
  return 0;
}
