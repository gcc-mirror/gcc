// { dg-do run  }
struct B
{
  virtual int f() volatile
    { return 1; }
};

struct D : public B 
{
  int f() 
    { return 0; }
};

struct D2 : public D
{
  int f()
    { return 2; } 
};

int main()
{
  D2 d2;
  D& d = d2;
  return d.f();
}
