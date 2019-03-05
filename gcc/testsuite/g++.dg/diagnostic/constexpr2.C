// { dg-do compile { target c++11 } }

extern constexpr int i __attribute__((unused));  // { dg-error "22:declaration of .constexpr. variable .i." }

struct S
{
  constexpr static int i __attribute__((unused));  // { dg-error "24:.constexpr. static data member .i." }
};
