// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_virtual.

#include <meta>

using namespace std::meta;

void func () {}

struct B {
  void nonvirt ();
  virtual void virt_no_override ();
  virtual void virt_implicit_override ();
  virtual void virt_explicit_override ();
  virtual void pure_virt () = 0;
  virtual ~B ();
};

struct D : B {
  void nonvirt ();
  void virt_implicit_override ();
  void virt_explicit_override () override;
  void pure_virt ();
  ~D ();
};

static_assert (!is_virtual (^^func));
static_assert (!is_virtual (^^B::nonvirt));
static_assert (is_virtual (^^B::virt_no_override));
static_assert (is_virtual (^^B::virt_implicit_override));
static_assert (is_virtual (^^B::virt_explicit_override));
static_assert (is_virtual (^^B::pure_virt));
static_assert (is_virtual (^^B::~B));

static_assert (!is_virtual (^^D::nonvirt));
static_assert (is_virtual (^^D::virt_no_override));
static_assert (is_virtual (^^D::virt_implicit_override));
static_assert (is_virtual (^^D::virt_explicit_override));
static_assert (is_virtual (^^D::pure_virt));
static_assert (is_virtual (^^D::~D));

struct E {
  ~E () = delete;
};

struct F {
  virtual ~F () = delete;
};

static_assert (!is_virtual (^^E::~E));
static_assert (is_virtual (^^F::~F));

struct B1 {};
struct B2 {};
struct D2 : B1, virtual B2 { };
constexpr auto ctx = access_context::unchecked ();

static_assert (!is_virtual (bases_of (^^D2, ctx)[0]));
static_assert (is_virtual (bases_of (^^D2, ctx)[1]));
