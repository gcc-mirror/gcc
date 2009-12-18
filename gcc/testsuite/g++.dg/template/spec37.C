// PR c++/28300

template<typename> struct A
{
    template<typename T> struct A<T*>; // { dg-error "namespace scope" }
};
