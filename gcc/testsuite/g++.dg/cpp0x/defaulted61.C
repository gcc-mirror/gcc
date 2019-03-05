// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN3OneD0Ev" } }

// PR C++/88114
// Destructor of an abstract class was never generated
// when compiling the class - nor later due to the
// '#pragma interface'

#pragma implementation
#pragma interface

class One
{
 public:
  virtual ~One() = default;
  void some_fn();
  virtual void later() = 0;
 private:
  int m_int;
};

void One::some_fn() { }
