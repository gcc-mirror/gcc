// PR c++/80485
// { dg-do compile { target c++11 } }

struct dummy {
  void nonnull() {};
  void nonnull2();
};

typedef void (dummy::*safe_bool)();

constexpr safe_bool a = &dummy::nonnull;
constexpr safe_bool b = &dummy::nonnull2;

static_assert( static_cast<bool>( a ), "" );
static_assert( static_cast<bool>( b ), "" );
