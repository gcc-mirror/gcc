// Build don't link: 
// Disable -ansi -pedantic-errors because of GNU extension
// Special g++ Options:                                             
template<class foo>
class bar {
public:
  void baz() { (({ while( foo::baz() );})); }
};
template<class foo>
void baz() { (({ while( foo::baz() );})); }
