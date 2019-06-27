// { dg-do compile { target c++98_only } }

void foo()
{
  auto void bar();  // { dg-error "3:storage class .auto. invalid for function" }
}
