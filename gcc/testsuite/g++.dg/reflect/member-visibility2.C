// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Tests std::meta::is_public, std::meta::is_private, std::meta::is_protected
#include <meta>

// Test bit-fields
struct S {
  unsigned char b1 : 3;
protected:
  unsigned char    : 2;
private:
  unsigned char b2 : 6;
};

constexpr auto ctx = std::meta::access_context::unchecked();
static_assert (std::meta::members_of (^^S, ctx).size() > 3);

static_assert (std::meta::is_bit_field (std::meta::members_of (^^S, ctx)[0]));
static_assert (std::meta::has_identifier (std::meta::members_of (^^S, ctx)[0]));
static_assert (std::meta::is_public (std::meta::members_of (^^S, ctx)[0]));

static_assert (std::meta::is_bit_field (std::meta::members_of (^^S, ctx)[1]));
static_assert (!std::meta::has_identifier (std::meta::members_of (^^S, ctx)[1]));
static_assert (std::meta::is_protected (std::meta::members_of (^^S, ctx)[1]));

static_assert (std::meta::is_bit_field (std::meta::members_of (^^S, ctx)[2]));
static_assert (std::meta::has_identifier (std::meta::members_of (^^S, ctx)[2]));
static_assert (std::meta::is_private (std::meta::members_of (^^S, ctx)[2]));

// Test inheritance and change of access modifier
class A {
protected:
  int protected_member;

  void protected_function();

  virtual void protected_virtual_function();

  void test() {
    static_assert (std::meta::is_protected (^^protected_member));
    static_assert (std::meta::is_protected (^^protected_function));
    static_assert (std::meta::is_protected (^^protected_virtual_function));
  }
};


class B : public A {
private:
  void protected_virtual_function() override;

  void test() {
    static_assert (std::meta::is_protected (^^B::protected_member));
    static_assert (std::meta::is_protected (^^B::protected_function));

    static_assert (std::meta::is_protected (^^A::protected_virtual_function));
    static_assert (std::meta::is_private (^^B::protected_virtual_function));
  }
};

// TODO: add more tests for inheritance and change of access modification
// TODO: add more tests for base access during multilevel inheritance
