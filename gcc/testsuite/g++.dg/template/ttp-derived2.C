// PR c++/42329

template <typename T1, typename T2>
class B {};

template <typename T>
class D : public B<T, T> {};

template <template <typename, typename> class U, typename T1, typename T2>
void g(U<T1, T2>*) {}

int main()
{
  D<long> dl;
  g(&dl); // error: no matching function for call to ‘g(D<long int>*)’
}
