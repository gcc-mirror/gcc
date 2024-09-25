// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed visibility
// visibility file
// From: Sandeep Shroff <ss@caere.com>
// Date:     Thu, 05 Aug 1993 17:23:20 -0700
// Subject:  Access to private constructor.
// Message-ID: <9308060023.AA10283@neptune.caere.com>
#include <iostream>
#include <cstring>

class Base
{
public:
  char* getName() {return name_;}

private:
  Base();
  Base(char* str);

  char* name_;
};

class Derived : public Base
{
public:
  Derived(int n, char* str);
  Derived(int n);

  int getNum() {return num_;}
private:
  int num_;
};

Base::Base() // { dg-message "private" }
{
  name_ = std::strcpy(new char[std::strlen(" ") + 1], " ");
}

Base::Base(char* str) // { dg-message "private" }
{
  if(str != NULL)
    name_ = std::strcpy(new char[std::strlen(str) + 1], str);
}

Derived::Derived(int n, char* str) : Base(str) // { dg-error "within this context" }
{
  num_ = n;
}

Derived::Derived(int n) : Base() // { dg-error "within this context" }
{
  num_ = n;
}



int main()
{
  // Derived* d = new Derived(10, "test");
  Derived* d = new Derived(10);

  std::cerr << d->getNum() << "\t" << d->getName() << std::endl;
}



