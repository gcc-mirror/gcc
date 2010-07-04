// { dg-options "-std=gnu++0x" }
template<typename..., typename> void foo(); // { dg-message "candidate" }

void bar()
{
  foo<int>(); // { dg-error "no matching function" }
}
