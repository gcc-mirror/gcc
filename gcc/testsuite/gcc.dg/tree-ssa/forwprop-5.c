/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -w" } */
/* { dg-options "-O1 -fdump-tree-optimized -w -msse" { target { i?86-*-* x86_64-*-* } } } */

#define vector __attribute__((vector_size(16) ))
struct VecClass
{
  vector float v;
};

vector float foo( vector float v )
{
    vector float x = v;
    x = x + x;
    struct VecClass disappear = *(struct VecClass*)&x;
    return disappear.v;
}

/* { dg-final { scan-tree-dump-times "disappear" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
