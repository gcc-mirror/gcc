// { dg-options "-std=c++0x" }

void f()
{
  auto val = val;  // { dg-error "auto" }
}
