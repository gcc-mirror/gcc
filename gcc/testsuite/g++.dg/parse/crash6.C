struct P {};
    
template <typename >
struct O 
{
    struct I;
};

template <typename T>
struct O<T>::I::S : P {}; // { dg-error "" }
