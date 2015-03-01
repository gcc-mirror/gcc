/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

#include <iostream>

using namespace std;

class Base {
   public:
     int My_V;
     virtual void Primitive ();

     Base ();
};

void Base::Primitive () {
  cout << "C++ Primitive  " << this->My_V << "\n";
}

Base::Base () {
}

void Dispatch (Base * B) {
  B->Primitive ();
}

/* { dg-final { cleanup-ada-spec } } */
