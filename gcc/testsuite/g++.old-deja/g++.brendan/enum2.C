// Build don't link: 
// GROUPS passed enums
class foo {
public:
  enum bar { baz = 1, bat = 7 };
};

class foo2 {
public:
  enum bar2 { baz2 = 1, bat2 = 7 };
};

class derv : public foo, public foo2 { };

int main()
{
  foo::bar x = foo::baz;
  derv::bar2 y = derv::bat2;
}
