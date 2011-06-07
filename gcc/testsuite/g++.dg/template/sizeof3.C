// The call to f is not potentially evaluated (3.2), so f<int> is not used,
// so it should not be required.

template <class T>
T f (T)
{
  typename T::X x;
}

int main()
{
  int i = sizeof (f(0));
}
