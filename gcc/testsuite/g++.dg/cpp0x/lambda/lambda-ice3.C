// PR c++/47795
// { dg-options "-std=c++0x" }

class Klass
{
  unsigned int local;
public:
  bool dostuff();
};

bool Klass::dostuff()
{
  auto f = []() -> bool {
    if (local & 1) { return true; } // { dg-error "not captured" }
    return false;
  };
}

int main()
{
  Klass c;
  return 0;
}
