// { dg-do link  }

struct B
{
  virtual ~B() {}
};

struct A : public B
{
  ~A();
  void foo(void);
  void bar(void);
};

inline void A::foo(void)
{
  static int i;
  i++;
}

void A::bar()
{
  foo();
}

int main()
{
}
