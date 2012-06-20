// PR c++/53651
// { dg-do compile { target c++11 } }

template<typename> struct wrap { void bar(); };

template<typename T> auto foo(T* t) -> wrap<T>* { return 0; }

template<typename T>
struct holder : decltype(*foo((T*)0)) // { dg-error "class type" }
{
    using decltype(*foo((T*)0))::bar; // { dg-error "is not a base" }
};

holder<int> h;
