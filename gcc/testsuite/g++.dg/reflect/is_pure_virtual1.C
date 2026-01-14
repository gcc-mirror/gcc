// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_pure_virtual.

#include <meta>

using namespace std::meta;

void func() {}

struct B {
  void nonvirt();
  virtual void virt_no_override();
  virtual void virt_implicit_override();
  virtual void virt_explicit_override();
  virtual void pure_virt() = 0;
  virtual ~B() = 0;
};

struct D : B {
  void nonvirt();
  void virt_implicit_override();
  void virt_explicit_override() override;
  void pure_virt();
  virtual ~D() = 0;
};

struct DD {
  ~DD() = delete;
};

static_assert (!is_pure_virtual (^^func));

static_assert (!is_pure_virtual (^^B::nonvirt));
static_assert (!is_pure_virtual (^^B::virt_no_override));
static_assert (!is_pure_virtual (^^B::virt_implicit_override));
static_assert (!is_pure_virtual (^^B::virt_explicit_override));
static_assert (is_pure_virtual (^^B::pure_virt));
static_assert (is_pure_virtual (^^B::~B));

static_assert (!is_pure_virtual (^^D::nonvirt));
static_assert (!is_pure_virtual (^^D::virt_no_override));
static_assert (!is_pure_virtual (^^D::virt_implicit_override));
static_assert (!is_pure_virtual (^^D::virt_explicit_override));
static_assert (!is_pure_virtual (^^D::pure_virt));
static_assert (is_pure_virtual (^^D::~D));

static_assert (!is_pure_virtual (^^DD::~DD));
