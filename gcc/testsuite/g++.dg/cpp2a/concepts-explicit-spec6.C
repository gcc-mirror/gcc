// { dg-do compile { target c++20 } }

template<typename T>
struct A {
    template<class T2> void f1(T, T2); // member template
    template<class T2> void f2(T, T2); // member template
};

template<>
template<class X1> void A<int>::f1(int, X1);

// Specialization with template-id
template<>
template<> void A<int>::f2<char>(int, char);

// Specialization with deduction
template<>
template<> void A<int>::f1(int, char);
