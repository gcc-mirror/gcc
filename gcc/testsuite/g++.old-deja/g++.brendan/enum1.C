// Build don't link: 
// GROUPS passed enums
class foo {
public:
  enum bar { baz = 1, bat = 7 };
};

class derv : public foo { };

main()
{
  foo::bar x = foo::baz;
  derv::bar y = derv::bat;
}
