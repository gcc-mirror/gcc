// Build don't link:
// GROUPS passed membertemplates
template <class T>
class S
{
  S(const S<T>& x)  {}

  template <class U>
  S(const S<U>& x)  {}
}; 
