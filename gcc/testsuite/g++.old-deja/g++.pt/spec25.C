// Build don't link:

template <class T, int I>
struct S {
};

template <int I>
struct S <double, I> {
};

template <class T>
void f ()
{
  S<double, T::x> s;
}
