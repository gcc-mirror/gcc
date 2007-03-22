// { dg-do assemble  }
// GROUPS passed old-abort
/*
   I received the following message when using g++ (version 2.3.3):

   main.cc: In method 'Implicit<implicit<INTEGER,2>,3>::Implicit()':
   main.cc: Internal compiler error 241.
   main.cc: Please report this to 'bug-g++@prep.ai.mit.edu'
   */

#include <iostream>

class INTEGER {
int x;
public:
   typedef int BASE;
   INTEGER(int y) : x(y) {}
   INTEGER() {}
   void encode() { std::cout << "Integer encoder";}
   int operator=(int y) { x=y; return x; }
   operator int() {return x; }
};

template< class T,  int n> class Implicit : public T {
   public:
     typedef typename T::BASE BASE;
     Implicit(BASE value ): T(value) {}
     Implicit() : T() {}
     int myTag() { return n; }
     void encode() { T::encode(); }
     BASE operator=(BASE t) { return T::operator=(t); }
};

int
main() 
{
  Implicit<Implicit<INTEGER, 2> ,  3> y;

  y = 10;
}



