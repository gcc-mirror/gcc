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

// { dg-output "contract violation in function Base::b at .*.C:11: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::b at .*.C:12: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Base::b at .*.C:13: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child::fun at .*.C:26: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child::fun at .*.C:27: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function Child::fun at .*.C:28: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function VChild::b at .*.C:37: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function VChild::b at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function VChild::b at .*.C:39: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TChild<Base>::fun at .*.C:49: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TChild<Base>::fun at .*.C:50: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TChild<Base>::fun at .*.C:51: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function PubChild::fun at .*.C:67: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function PubChild::fun at .*.C:68: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function PubChild::fun at .*.C:69: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TPubChild<PubBase>::fun at .*.C:79: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TPubChild<PubBase>::fun at .*.C:80: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TPubChild<PubBase>::fun at .*.C:81: .*(\n|\r\n|\r)" }

