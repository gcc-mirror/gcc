// Make sure type deduction works for both types of array parameters.
template <class T> void f (T (&a)[2]) { }
template <class T> void g (T a[2]) { }
int main()
{
  int a[2] = { 0, 0 };
  f (a);
  g (a);
}
