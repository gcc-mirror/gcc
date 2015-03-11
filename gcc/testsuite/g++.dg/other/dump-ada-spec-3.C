/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

using namespace std;

class Base {
   public:
     int My_V;
     virtual void Primitive ();

     Base ();
};

void Base::Primitive () {
}

Base::Base () {
}

void Dispatch (Base * B) {
  B->Primitive ();
}

/* { dg-final { cleanup-ada-spec } } */
