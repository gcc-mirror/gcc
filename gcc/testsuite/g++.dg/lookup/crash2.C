// PR c++/14086

struct ClassA
{
  ClassA();
};

struct ClassB
{
  enum Enum {ClassB}; // { dg-error "" }
  ClassA key;

           ClassB();
  virtual ~ClassB();
};


ClassB::ClassB()
{
}
