// { dg-do compile }

// Origin: Debian GCC maintainers <debian-gcc@lists.debian.org>
//	   Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/16706: Dependent type calculation during access checking

template<typename> struct A 
{
    A();
    template<typename> struct X {};
};

template<typename T> struct B
{
    typename A<T>::template X<int> x;
    template<typename> struct C;
};

template<typename T> template<typename U> struct B<T>::C<U*>
{
    C() {}
    A<int> a;
};

template struct B<int>::C<int*>;
