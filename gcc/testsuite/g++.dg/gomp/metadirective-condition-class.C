/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

class c
{
 public:
  int x;
  c (int xx) { x = xx; }
  operator bool() { return x != 0; }
};

void f (c &objref)
{
  #pragma omp metadirective when(user={condition(objref)} : nothing) otherwise(nothing)
}


template <typename T> class d
{
 public:
  T x;
  d (T xx) { x = xx; }
  operator bool() { return x != 0; }
};

template <typename T>
void g (d<T> &objref)
{
  #pragma omp metadirective when(user={condition(objref)} : nothing) otherwise(nothing)
}

int main (void)
{
  c obj1 (42);
  d<int> obj2 (69);

  f (obj1);
  g (obj2);
}

/* { dg-final { scan-tree-dump "c::operator bool \\(\\(struct c .\\) objref\\)" "original" } } */

/* { dg-final { scan-tree-dump "d<int>::operator bool \\(\\(struct d .\\) objref\\)" "original" } } */
