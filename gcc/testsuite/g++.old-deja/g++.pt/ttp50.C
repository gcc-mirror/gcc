// { dg-do run  }
template <class T, template <T> class TT> class C {};
template <int> class D {};

int main()
{
  C<int,D> c;
}
