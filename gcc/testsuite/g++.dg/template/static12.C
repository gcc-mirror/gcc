// PR c++/18470

template<typename> struct A
{
    static const int i=1;
};

template<typename T> struct B : A<T>
{
    using A<T>::i;
    char s[i];       // fails
    char t[A<T>::i]; // compiles
};
