// Build don't link:

template<typename T>
void f()
{
  typename T::u;  // ERROR - declare anything
}
