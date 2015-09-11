// { dg-do assemble  }
// GROUPS passed ARM-compliance
// arm file
// From: belley@cae.ca (Benoit Belley 3218)
// Subject: Bad access control with private constructor and derivation
// Date: Fri, 28 May 1993 12:39:57 -0400 (EDT)

#include <iostream>

class X
{
public:
  void f();

private:
  X();
};

class Y : public X
{
public:
  Y();
};

X::X() // { dg-message "private" }
{
  std::cout << "X::X()" << std::endl;
}

void X::f()
{
  std::cout << "X::f()" << std::endl;
}

Y::Y() // { dg-error "within this context" }
{
  std::cout << "Y::Y()" << std::endl;
}


int main()
{
  Y y;
  y.f();
}




