/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized" } */

int c0 = 0;
typedef int v4si __attribute__((vector_size(4*sizeof(int))));
v4si a;
int f()
{
        return a[c0];
}
int g()
{
        return a[c0];
}

/* { dg-final { scan-ipa-dump "optimized: Semantic equality hit:f/\[0-9+\]+->g/\[0-9+\]+" "icf" } } */
