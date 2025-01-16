/* { dg-do compile } */
/* { dg-options "-O2 -flive-patching=inline-only-static -fdump-ipa-inline" } */

extern int sum, n, m;

int foo (int a)
{
  return a + n;
}

static int bar (int b)
{
  return b * m;
}

int main()
{
  sum = foo (m) + bar (n); 
  return 0;
}

/* { dg-final { scan-ipa-dump "foo/1 function has external linkage when the user requests only inlining static for live patching"  "inline" } } */
