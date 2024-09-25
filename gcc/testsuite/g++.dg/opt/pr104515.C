// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-lim2-details" }

using T = int;
struct Vec {
  T* end;
};
void pop_back_many(Vec& v, unsigned n)
{
  for (unsigned i = 0; i < n; ++i) {
    --v.end;
    //  The end-of-object clobber prevented store motion of v
    v.end->~T();
  }
}

// { dg-final { scan-tree-dump "Executing store motion of v" "lim2" } }
// { dg-final { scan-tree-dump "Re-issueing dependent" "lim2" } }
