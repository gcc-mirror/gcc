// { dg-do run  }
// prms-id: 2846

extern "C" int printf(const char *, ...);
extern "C" void exit(int);

class A;
class B;

int c;

class A {
public:

  A(void){}
  A(const A&){}
  A(const B&);

  virtual ~A(void){}

  virtual void print(void) const {
    printf("A::print\n");
    printf("FAIL\n");
    exit(1);
  }
  B compute(void) const;
};

class B : private A {
friend class A;
public:

  virtual ~B(void){}

  void print(void) const {
    ++c;
    printf("B::print\n");
  }

private:
  B(const A& x, int){}
};

A::A(const B& s) {
  s.print();
}

B A::compute(void) const {
  B sub(*this, 1);
  return sub;
}

int main ()
{
  A titi;
  A toto = titi.compute();
  if (c != 1)
    {
      printf ("FAIL\n");
      return 1;
    }
  else
    {
      printf("PASS\n");
      return 0;
    }
}
