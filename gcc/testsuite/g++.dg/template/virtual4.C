// PR c++/56243

struct A
{
  virtual int String ();
};

struct F: A { };

struct G
{
  F value;
};

struct D
{
  template <int>
  void Verify()
  {
    G x;
    F& name = x.value;
    name.String();
  }
};

int main()
{
  D d;
  d.Verify<42>();
}
