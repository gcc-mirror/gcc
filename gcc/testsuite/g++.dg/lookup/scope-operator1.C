/* PR c++/250 */
/* { dg-do compile } */

template <class T> void Bar(T *p)
{
}
 
template <class T> class Foo
{
public:
  Foo(T *p) { Bar(p); }
  // The global scope operator wasn't respected in this case under gcc 3.0
  void Bar(T *p) { ::Bar<T>(p); }
};

int main()
{
  double* d;
  Foo<double> f(d);
}
