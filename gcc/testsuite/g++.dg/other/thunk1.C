// PR c++/12007 Multiple inheritance float pass by value fails
// { dg-do run }
// { dg-additional-options "-fexcess-precision=fast" }

extern "C" void abort (void);

class gvImpl
{
public:
  virtual void PutVal(float value){}
};

class foo { public: virtual void Bar(){} };

class myGv: public foo, public gvImpl
{
  void PutVal(float value){ if (value != 3.14159f) abort (); }
};

myGv x;
gvImpl* object = &x;

int main()
{
  object->PutVal(3.14159f);
  return 0;
}
