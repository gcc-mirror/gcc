// { dg-do compile }
// Contributed by: Wolfgang Bangerth <bangerth at dealii dot org>
// PR c++/14409: Accepts invalid function signature for explicit instantiation

struct X 
{ 
    template <typename U> 
    void foo (U) {}

    template <typename U> 
    void foo_const (U) const {}
};

template void X::foo (int); 
template void X::foo_const (int) const; 

template void X::foo (int) const;   // { dg-error "" }
template void X::foo_const (int);   // { dg-error "" }
