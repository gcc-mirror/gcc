// { dg-options "-std=c++11" }

void f()
{
  auto val = val;  // { dg-error "auto" }
}
