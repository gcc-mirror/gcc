// PR middle-end/17525

struct A
{
  ~A();
  int a;
};

struct B : public A
{
  virtual ~B();
};

void run (B& b, B& b1)
{
  b1 = b;
}
