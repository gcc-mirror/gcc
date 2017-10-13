template <class T> void f()
{
  static_cast<int&>(42);	// { dg-error "static_cast" }
}
