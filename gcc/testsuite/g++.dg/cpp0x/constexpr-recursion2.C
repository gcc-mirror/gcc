// PR c++/70344
// { dg-do compile { target c++11 } }
// { dg-options -O }

struct Z
{
  Z () = default;
  Z (Z const &) = default;
  constexpr Z (Z &&) {}
};

constexpr int
fn (Z v)
{
  return fn (v);
}

auto t = fn (Z ());
