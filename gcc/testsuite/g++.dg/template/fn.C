// instantiation cannot turn a data member into a function!

typedef int (frib) (int);

template <typename T> class X 
{
  T v; // { dg-error "declared function" }
};

X<frib> v;
