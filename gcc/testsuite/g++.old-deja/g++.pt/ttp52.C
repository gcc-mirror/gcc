// Build don't link:

template<class A,class B> class mymap {};

template<class Key, 
         class Value, 
         template<class, class > class MapT> 
class base 
{
  
};

// specialization
template<class Key, class Value>
class base<Key, Value, mymap<int, int > >	
{						// ERROR - type/value mismatch
  
};
