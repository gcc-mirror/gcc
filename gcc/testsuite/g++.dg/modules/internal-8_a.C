// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }
// Note we don't support eliding non-ODR usages within templates,
// as this is in general impossible to detect.
// FIXME we could probably at least elide cases that we can prove, though...

export module M;

namespace {
  struct internal_t {};
  struct A { int x; };
  struct B { A a; };
};
static const int value = 123;
static const int& ref = 456;
static const internal_t internal {};
static constexpr B other { 789 };
static constexpr const B& other_ref = other;
static const B& other_ref_ref = (0, other_ref);

constexpr void f(int) {}

constexpr int no_odr_use_cexpr() {
  int x = value;
  int y = ref;
  int z = (internal, 0);

  value;
  ref;
  internal;
  bool b = value < value;
  f(other_ref_ref.a.x);
  return value;
}

struct S {};
static constexpr int S::* md = nullptr;
static constexpr void (S::* mfn)() = nullptr;
constexpr auto test_md() {
  auto pfn = md;
  return pfn;
}
constexpr auto test_mfn() {
  auto pfn = mfn;
  return pfn;
}

namespace {
  struct Bitfield {
    int x : 5;
  };
  constexpr Bitfield bf{ 4 };
}
constexpr auto test_bitfield() {
  return bf.x;
}

// PR c++/119097
namespace { static constexpr const int default_val { 789 }; }
struct A {
  int value { default_val };
  constexpr auto a() { return default_val; }
};

