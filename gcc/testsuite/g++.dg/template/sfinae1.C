// PR c++/14337

template <bool> struct Constraint; 
template <>     struct Constraint<true> { typedef int Result; }; 
 
template <typename T> 
struct IsInt      { static const bool value = false; }; 
 
template <> 
struct IsInt<int> { static const bool value = true; }; 
 
template <typename T> 
typename Constraint<IsInt<T>::value>::Result foo(T); 
 
template <typename T> 
typename Constraint<!IsInt<T>::value>::Result foo(T); 
 
template <typename> 
void bar() { 
    foo(1); 
} 
