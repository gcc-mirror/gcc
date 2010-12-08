// { dg-options "-std=gnu++0x" }
template<typename, typename..., typename> void foo(); // { dg-message "note" }

void bar()
{
  foo<int>(); // { dg-error "no matching function" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 6 }
}
