// PR c++/47795
// { dg-do compile { target c++11 } }

class Klass
{
  unsigned int local;		// { dg-error "non-static" }
public:
  bool dostuff();
};

bool Klass::dostuff()
{
  auto f = []() -> bool {
    if (local & 1) { return true; } // { dg-error "not captured|this location" }
    return false;
  };
}

int main()
{
  Klass c;
  return 0;
}
