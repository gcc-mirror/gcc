// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <cstdlib>
#include <iostream>

class Vector {
private:
  int *p;
  int sz;

public:
  // Exception class
  class Range     {
  private:
    int value_i;

  public:
    Range( int i ) { value_i = i; };
    int value() { return value_i; };
  };

  Vector( int s );
  ~Vector();
  int  size() { return sz; };
  int& operator []( int i );
};

Vector::Vector(int s) {
  sz = s;
  p = new int[sz];
}

Vector::~Vector() {
  delete [] p;
}

int&
Vector::operator [](int i) {
  if (0<=i && i<sz) {
    return p[i];
  } else {
    throw Range( i );
  }
}

void crash(Vector& v ) {
  v[v.size()+10];  // Triggers range error!
}

void do_something(Vector& v) {
  crash( v );
}

void
f(Vector& v) {
  try {
    do_something( v );
  } catch (Vector::Range& r) {
    std::cout << "Invalid vector range " << r.value()
      << " caught in f()" << std::endl;
    std::exit(0);
  }
}

main() {
  Vector v(10);

  f( v );
  return 1;
}



