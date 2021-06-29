// ensure that that preconditions can access public, protected, and private
// members of the current and base classes
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

struct Base
{
  int pub{-1};

  virtual int b()
    [[ pre: pub > 0 ]]
    [[ pre: pro > 0 ]]
    [[ pre: pri > 0 ]]
  {
    return pub * pro * pri;
  }

  protected:
    int pro{-1};
    int pri{-1};
};

struct Child : Base
{
  int fun()
    [[ pre: pub > 0 ]]
    [[ pre: pro > 0 ]]
    [[ pre: pri > 0 ]]
  {
    return pub * pro;
  }
};

struct VChild : Base
{
  int b()
    [[ pre: pub > 0 ]]
    [[ pre: pro > 0 ]]
    [[ pre: pri > 0 ]]
  {
    return pub * pro;
  }
};

template<typename B>
struct TChild : B
{
  int fun()
    [[ pre: B::pub > 0 ]]
    [[ pre: B::pro > 0 ]]
    [[ pre: B::pri > 0 ]]
  {
    return B::pub * B::pro;
  }
};

struct PubBase
{
  int pub{-1};
  int pro{-1};
  int pri{-1};
};

struct PubChild : PubBase
{
  int fun()
    [[ pre: pub > 0 ]]
    [[ pre: pro > 0 ]]
    [[ pre: pri > 0 ]]
  {
    return pub * pro;
  }
};

template<typename B>
struct TPubChild : B
{
  int fun()
    [[ pre: B::pub > 0 ]]
    [[ pre: B::pro > 0 ]]
    [[ pre: B::pri > 0 ]]
  {
    return B::pub * B::pro;
  }
};

int main()
{
  Base base{};
  base.b();

  Child child{};
  child.fun();

  VChild vchild{};
  vchild.b();

  TChild<Base> tchild{};
  tchild.fun();

  PubChild pubchild{};
  pubchild.fun();

  TPubChild<PubBase> tpubchild;
  tpubchild.fun();

  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 11 Base::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 12 Base::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 13 Base::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 26 Child::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 27 Child::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 28 Child::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 37 VChild::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 38 VChild::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 39 VChild::b .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 49 TChild<Base>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 50 TChild<Base>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 51 TChild<Base>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 67 PubChild::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 PubChild::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 69 PubChild::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 79 TPubChild<PubBase>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 80 TPubChild<PubBase>::fun .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 81 TPubChild<PubBase>::fun .*(\n|\r\n|\r)*" }

