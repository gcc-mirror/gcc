/* { dg-do link } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized -flto -fdump-tree-optimized -fno-ipa-vrp" } */
/* { dg-require-effective-target lto } */
/* { dg-additional-sources "ipa-icf-38a.c" }*/

/* Based on ipa-icf-3.c.  */

typedef int v4si __attribute__ ((vector_size (16)));

__attribute__ ((noinline))
int foo(void)
{
  v4si a = {1,2,3,4};
  v4si b = {3,2,1,4};
  v4si c;

  return 54;
}

extern int bar(void);

int main()
{
  int volatile a = foo();
  int volatile b = bar();

  return 0;
}

/* { dg-final { scan-wpa-ipa-dump "Semantic equality hit:foo/\[0-9+\]+->bar/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-wpa-ipa-dump "Equal symbols: 1" "icf"  } } */
/* { dg-final { scan-ltrans-tree-dump "Function foo" "optimized" } } */
/* { dg-final { scan-ltrans-tree-dump-not "Function bar" "optimized" } } */
