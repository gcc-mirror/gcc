struct B
{
  virtual int f() volatile
    { return 0; }
};


struct D : public B 
{
  virtual int f()
    { return 1; }
};

int main()
{
  volatile D d;
  volatile B& b = d;
  return b.f();
}
