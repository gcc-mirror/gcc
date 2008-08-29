/* Verify that modification analysis detects modfications.  */
/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-inline-details -fno-early-inlining"  } */

struct whatever
{
  int first;
  unsigned second;
};

void func1 (struct whatever w);
void func2 (struct whatever *pw);
void func3 (int i);
void func4 (int *pi);

void the_test (struct whatever u, struct whatever v,
	       struct whatever w, struct whatever x,
	       int i, int j, int k, int l)
{
  struct whatever *pw = &w;
  int *pk = &k;

  j = l+3;
  v.first = 9;

  func1 (u);
  func1 (v);
  func2 (pw);
  func2 (&x);
  func3 (i);
  func3 (j);
  func4 (pk);
  func4 (&l);
}

/* { dg-final { scan-ipa-dump-not "param 0\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 1\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 2\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 3\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump-not "param 4\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 5\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 6\[^\\n\]*modified" "inline" } } */
/* { dg-final { scan-ipa-dump "param 7\[^\\n\]*modified" "inline" } } */
/* { dg-final { cleanup-ipa-dump "inline" } } */
