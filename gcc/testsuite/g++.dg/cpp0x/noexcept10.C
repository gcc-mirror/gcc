// PR c++/49082
// { dg-options -std=c++0x }

namespace std { template <class T> T&& declval() noexcept; }

struct Base
{
  Base(const Base&) noexcept(false);
  Base(Base&&) noexcept(false);
 ~Base() noexcept(false);
};

struct Derived
: Base
{
  // Derived(const Derived&) = default;
  // Derived(Derived&&) = default;
};

static_assert(!noexcept(Base(std::declval<const Base&>())), "Error");
static_assert(!noexcept(Derived(std::declval<const Derived&>())), "Error"); // Error

static_assert(!noexcept(Base(std::declval<Base&&>())), "Error");
static_assert(!noexcept(Derived(std::declval<Derived&&>())), "Error"); // Error

static_assert(!noexcept(std::declval<Base&>().~Base()), "Error"); // OK
static_assert(!noexcept(std::declval<Derived&>().~Derived()), "Error"); // Error
