// PR c++/15890

template < typename T >
void operator delete ( void* raw ) { // { dg-error "" }
  delete raw;
}

class A { };

int main() {
  A* a = new A;
  delete a;
}

