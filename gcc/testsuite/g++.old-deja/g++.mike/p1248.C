// GROUPS passed pure-virt
extern "C" int printf (const char *, ...);
class Base {
public:
  virtual ~Base() =0;
};

class Deranged : public Base {
public:
  int value;
  virtual ~Deranged();
};


Deranged::~Deranged(){}

void foo() {
  Deranged d;
}

int main()
{
  foo();
  printf("PASS\n");
  return 0;
}

Base::~Base () { }
