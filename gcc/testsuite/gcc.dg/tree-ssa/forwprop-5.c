/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1 -w" } */

#define vector __attribute__((vector_size(16) ))
struct VecClass
{
  vector float v;
};

vector float foo( vector float v )
{
    vector float x = v;
    x = x + x;
    struct VecClass y = *(struct VecClass*)&x;
    return y.v;
}

/* We should be able to convert the cast to a VCE in forwprop1. */
/* { dg-final { scan-tree-dump-times "VIEW_CONVERT_EXPR" 1 "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */

