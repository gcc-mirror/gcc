// Build don't link: 
// GROUPS passed miscellaneous
//The program listed below produces the following error during compilation:
//   % g++ bug17.cc
//   bug17.cc: In method `class Y& Y::operator = (const class Y&)':
//   bug17.cc:18: invalid use of non-lvalue array

class X {
public:
   X& operator=(const X&) { return *this; }
};

struct S {
   char c[10];
   X x;
};

class Y {
   S s;
public:
   const S& f() const { return s; }

   Y& operator=(const Y& _Y) {
      s = _Y.s;    // this line compiles
      s = _Y.f();  // this line does not compile
      return *this;
   }
};
