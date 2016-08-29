// PR c++/77379
// { dg-options "-fabi-version=0 -Wabi=10" }

struct __attribute ((abi_tag ("bar"))) string { };

struct Mother
{
  virtual ~Mother() {};
  int bar;
};

struct Father
{
  virtual string get_foo()  = 0;
};

class Derived:
  public Mother,
  public Father
{
public:
  string get_foo();		// { dg-warning "mangled name" }
};

struct Final:
    public Derived
{
};

int main()
{
  Final().get_foo();
}

// { dg-final { scan-assembler "_ZThn16_N7Derived7get_fooB3barEv" } }
