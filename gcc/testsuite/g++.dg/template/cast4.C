template <class T> void f()
{
  static_cast<int&>(42);	// { dg-error "3:invalid .static_cast." }
}
