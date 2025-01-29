// PR c++/117778
// { dg-do compile { target c++20 } }

int f1 (auto fp(auto fp2())); // { dg-error ".auto. parameter not permitted" }
int f2 (auto fp(auto fp2() -> auto)); // { dg-error ".auto. parameter not permitted" }
auto f3 (auto fp() -> auto) -> auto;
auto f3 (auto fp(auto fp2() -> auto) -> auto) -> auto; // { dg-error ".auto. parameter not permitted" }

void
g ()
{
  extern int e1 (auto fp()); // { dg-error ".auto. parameter not permitted" }
  extern int e2 (auto fp() -> auto); // { dg-error ".auto. parameter not permitted" }
  extern int e3 (auto fp(auto fp2() -> auto) -> auto); // { dg-error ".auto. parameter not permitted" }
}
