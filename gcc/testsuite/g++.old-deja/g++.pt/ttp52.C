// { dg-do assemble  }

template<class A,class B> class mymap {};

template<class Key, 
         class Value, 
         template<class, class > class MapT> 
class base 
{
};

// specialization
template<class Key, class Value>
class base<Key, Value, mymap<int, int > > // { dg-error "type/value|class template" }
{
};
