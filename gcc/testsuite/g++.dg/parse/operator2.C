// PR c++/7906
// Origin: Marcel Loose <loose@astron.nl>
// { dg-do compile }

template <typename> struct A { typedef int X; };

template <typename T> struct B
{
    typedef A<T> Y;
    struct C { operator typename Y::X() const; };
};

template <typename T> B<T>::C::operator typename B<T>::Y::X() const { return 0; }

B<int> b;
