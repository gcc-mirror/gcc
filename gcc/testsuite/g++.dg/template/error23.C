// PR c++/29632

struct nullptr_type {

  nullptr_type ( void ) {}

  template < typename T >
  operator T* ( void ) const {
    return ( 0 );
  }
} const nullptr;

int main ( void ) {
  0 == nullptr; // { dg-error "match" }
}


