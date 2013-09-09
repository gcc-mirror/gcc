class A
{
 protected:
  A();
  virtual ~A();
};

class B : virtual public A
{
 public:
  B();
  virtual ~B();
};

class C
{
 private:
  class C2 : public B
   {
   public:
     C2();
     virtual ~C2();
   };
};
