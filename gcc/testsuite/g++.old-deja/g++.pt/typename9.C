// Test to make sure that implicit typename doesn't break name binding rules.
// Special g++ Options: -w

typedef double A;
template<class T> class B {
  typedef char A;
};
template<class T> struct X : B<T> {
  A a;
};

int main()
{
  X<char*> x;
  return sizeof (x.a) != sizeof (double);
}
