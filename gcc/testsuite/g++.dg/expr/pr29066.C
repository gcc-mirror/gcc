// PR c++/29066
// Test pointer to member function comparison
// { dg-do run }

extern "C" void abort (void);

struct X
{
  virtual void a(void)=0;
};

struct Z : public X
{
  void a(void) {}
};


void f(X *obj)
{
  void (X::*xp)(void) = 0;
  void (X::*xp2)(void) = 0;

  xp = &X::a;

  if (xp == xp2)
    {
      abort(); 
    } 

  if (xp == 0)
    {
      abort();
    }
}

int main(int argc, char* argv[])
{
  Z myobj;

  f(&myobj);
  return 0;
}
