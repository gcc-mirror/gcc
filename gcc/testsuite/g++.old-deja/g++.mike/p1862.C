// GROUPS vtable
extern "C" int printf (const char *, ...);

class A
{
public:
  virtual ~A(){};
  virtual int type(void)
  {
    return -1;
  }
};

class B
{
public:
  virtual ~B(){};
};


class C0 : public B, public A
{
public:
  virtual int type(void)
  {
    return 0;
  }
};

class C1 : public C0
{
public:
  virtual int type(void)
  {
    return 1;
  }
};

class C2 : public C0
{
public:
  virtual int type(void)
  {
    return 2;
  }
};

int main()
{
  C1 *one = new C1;
  
  if (one->type() == 1)
    {
      printf ("PASS\n");
      return 0;
    }
  else
    {
      printf ("FAIL\n");
      return 1;
    }
}
