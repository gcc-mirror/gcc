// PR c++/91353 - P1331R2: Allow trivial default init in constexpr contexts.
// { dg-do compile { target c++20 } }

struct S { int i; };

constexpr void
fn ()
{
  S s;

  []() constexpr {
    int i;
  }();
}

constexpr int
fn2 ()
{
  return __extension__ ({ int n; n; }); // { dg-error "not usable in a constant expression" }
}

constexpr int i = fn2 (); // { dg-message "in .constexpr. expansion of" }
