// { dg-do assemble  }
// { dg-options "-Wnon-virtual-dtor -Weffc++" }
// 981203 bkoz
// g++/15309

class bermuda {  // { dg-warning "" } // WARNING -
public:
  virtual int func1(int); 
  ~bermuda();
};
