// Build don't link:

template <class T>
class S
{
  template <class U>
  class S2 {
    S2(const S2<U>& s2u) {}
  };
};
 
