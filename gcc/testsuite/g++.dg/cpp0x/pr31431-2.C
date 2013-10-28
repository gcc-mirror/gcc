// { dg-options "-std=gnu++11" }
template<typename, typename..., typename> void foo(); // { dg-message "note" }

void bar()
{
  foo<int>(); // { dg-error "no matching function" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 6 }
}
