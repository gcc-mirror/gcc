// Build don't link: 
template<class foo>
class bar {
public:
  void baz() { (({ while( foo::baz() );})); }
};
template<class foo>
void baz() { (({ while( foo::baz() );})); }
