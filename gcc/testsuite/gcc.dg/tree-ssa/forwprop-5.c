/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-esra -w" } */

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

/* We should be able to remove the intermediate struct and directly
   return x.  As we do not fold VIEW_CONVERT_EXPR<struct VecClass>(x).v
   that doesn't happen right now.  */
/* { dg-final { scan-tree-dump-times "VIEW_CONVERT_EXPR" 1 "esra"} } */
/* { dg-final { cleanup-tree-dump "esra" } } */
