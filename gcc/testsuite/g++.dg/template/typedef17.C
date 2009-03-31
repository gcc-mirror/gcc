// PR c++/37806

extern "C" int printf (const char *, ...);

template <typename T>
struct S1
{
  typedef void (function_type)(int) const;
};


struct S2: public S1<int>
{
  virtual function_type f = 0;
};

struct S3: public S2
{
  void 
  f (int i) const
  {
    printf ("Hello world: %d\n", i);
  }
};


int
main()
{
  S3 s;
  s.f(5);
}
