// PR c++/57253
// { dg-require-effective-target c++11 }

template<typename T> struct foo;

template<> struct foo<void()&> {};
template<> struct foo<void()> {};

int main()
{}
