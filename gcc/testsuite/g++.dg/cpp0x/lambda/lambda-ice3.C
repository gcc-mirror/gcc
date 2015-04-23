// PR c++/47795
// { dg-do compile { target c++11 } }

class Klass
{
  unsigned int local;		// { dg-message "" }
public:
  bool dostuff();
};

bool Klass::dostuff()
{
  auto f = []() -> bool {
    if (local & 1) { return true; } // { dg-error "" }
    return false;
  };
}

int main()
{
  Klass c;
  return 0;
}
