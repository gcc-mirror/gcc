// Build then link:
// Special g++ Options: -frepo

template <class T>
struct S {
  ~S ();
};

template <class T>
S<T>::~S () {}

int main ()
{
  S<int> s;
}
