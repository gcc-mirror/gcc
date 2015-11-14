// PR c++/35333

void foo(__complex__ double x)
{
  __builtin_conj(x)(); // { dg-error "function" }
}
