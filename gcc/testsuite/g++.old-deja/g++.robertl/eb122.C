// { dg-do assemble  }
// { dg-options "" }
// Disable -ansi -pedantic-errors because of GNU extension
template<class foo>
class bar {
public:
  void baz() { (({ while( foo::baz() );})); }
};
template<class foo>
void baz() { (({ while( foo::baz() );})); }
