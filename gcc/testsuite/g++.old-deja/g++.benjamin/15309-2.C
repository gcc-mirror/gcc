// 981203 bkoz
// g++/15309
// Build don't link: 
// Special g++ Options: -Wnon-virtual-dtor -Weffc++

class bermuda {  // WARNING - // WARNING -
public:
  virtual int func1(int); 
  ~bermuda();
};
