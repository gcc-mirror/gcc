// P2647R1 - Permitting static constexpr variables in constexpr functions
// { dg-do compile { target c++23 } }

constexpr char
test ()
{
  static const int x = 5;
  static constexpr char c[] = "Hello World";
  return *(c + x);
}

static_assert (test () == ' ');
