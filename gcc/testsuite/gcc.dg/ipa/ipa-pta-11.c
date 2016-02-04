/* { dg-do link } */
/* { dg-options "-O2 -fipa-pta -fdump-ipa-pta2-details" } */

static int i;
/* i should not escape here, p should point to i only.  */
/* { dg-final { scan-ipa-dump "p = { i }" "pta2" } } */
static int *p = &i;

int j;
/* q should point to j only.  */
/* { dg-final { scan-ipa-dump "q = { j }" "pta2" } } */
static int *q = &j;

static int k;
/* k should escape here, r should point to NONLOCAL, ESCAPED, k.  */
int *r = &k;
/* { dg-final { scan-ipa-dump "r = { ESCAPED NONLOCAL k }" "pta2" } } */

int l;
/* s should point to NONLOCAL, ESCAPED, l.  */
int *s = &l;
/* { dg-final { scan-ipa-dump "s = { ESCAPED NONLOCAL l }" "pta2" } } */

/* Make p and q referenced so they do not get optimized out.  */
int foo() { return &p < &q; }

int main()
{
  return 0;
}

/* It isn't clear if the escape if l is strictly necessary, if it were
   we should have i, r and s in ESCAPED as well.  */

/* { dg-final { scan-ipa-dump "ESCAPED = { ESCAPED NONLOCAL l k }" "pta2" } } */
