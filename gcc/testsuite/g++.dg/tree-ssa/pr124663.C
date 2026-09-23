// PR c++/124663
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-forwprop3-raw" }

#define vector __attribute__((vector_size(4*sizeof(int))))
void f(vector int *a)
{
  vector int cst = {1,2,3,4};
  vector int t = (*a == cst);
  *a = (t ? *a : cst);
}

// { dg-final { scan-tree-dump-not "_expr" "forwprop3" } }
