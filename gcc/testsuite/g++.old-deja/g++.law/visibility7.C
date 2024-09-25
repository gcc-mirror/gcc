// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed visibility
// visibility file
// From: Gordon Joly <G.Joly@cs.ucl.ac.uk>
// Date:     Wed, 21 Apr 93 09:42:07 +0100
// Subject:  /*** BUG REPORT : THE MYTH OF PRIVATE INHERITANCE ***/
// Message-ID: <9304210842.AA01815@life.ai.mit.edu>
#include <iostream>

class A {
 private:
  int number;
 public:
  A(int i) : number(i)
    {}
  virtual ~A()
    {}
  virtual void Number(int c) // { dg-message "declared" }
    { number = c; }
  virtual int Number() // { dg-message "declared" }
    { return number; }
};

class B : private A {
 private:
  int second_number;
 public:
  B(int c, int i) : second_number(c), A(i)
    {}
  virtual ~B()
    {}

  virtual void firstNumber(int b)  // renames member function Number(int) of class A
    { A::Number(b); }
  virtual int firstNumber()  // renames member function Number() of class A
    { return A::Number(); }
};




class C {
 private:
  B* bobject;
 public:
  C(B* bp) : bobject(bp)
    {}
  virtual ~C()
    {}
  //
  // the following two functions access
  // private member functions of class B
  // and they should not be able to do so
  //
  virtual void setBValue(int i) 
    { if (bobject) bobject->Number(i); } // { dg-error "this context|accessible base" }
  virtual int getBValue()
    { if (bobject) { return bobject->Number(); } return 0; } // { dg-error "this context|accessible base" }
};


int main()
{
  B* bobject = new B(2, 1);
  C* cobject = new C(bobject);
  cobject->setBValue(8);
  std::cout << cobject->getBValue() << std::endl;
  delete bobject;
  delete cobject;
}



