// Build don't link:
template <class Key>
class d0om_Hashmap
{
public:
  typedef int value_type;

  class iterator
  {
  public:
    value_type* operator-> () const;
  };

};


template <class Key>
d0om_Hashmap<Key>::value_type* d0om_Hashmap<Key>::iterator::operator-> () const
{
  return 0;
}
