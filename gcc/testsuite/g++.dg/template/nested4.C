template <typename T> struct A { 
    template<typename S> struct B { typedef A<S> X; }; 
 
}; 
 
template<typename> void f() { 
    typedef A<int>::B<double>::X X; 
} 
 
template void f<int> ();
