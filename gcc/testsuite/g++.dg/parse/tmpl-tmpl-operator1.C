// PR c++/8857
// Origin: Martin v. Loewis <loewis@informatik.hu-berlin.de>
// { dg-do compile }

template <typename T> struct A
{
    template <typename U> operator U() { return sizeof(U); }
};

template <typename T> struct B
{
    template <template <typename U> class X> operator X<double>() { return X<double>(); }
};

int main()
{
  A<double> a;
  B<long> b;
  a = b;
}
