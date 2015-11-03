// PR c++/66781

class foo
{
public:
  enum foo::bar{};  // { dg-error "does not name an enumeration" }
  foo::bar baz;
};
