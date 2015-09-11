// PR c++/24782

class Foo { public:  typedef int type1; };
class Bar { private: typedef Foo type2; }; // { dg-message "private" } 
void g(Bar::type2::type1) {} // { dg-error "context" }
