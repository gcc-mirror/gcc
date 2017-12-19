// PR c++/81061

const int i = 0;

void foo()
{
  (0, i) = 1;  // { dg-error "read-only" }
}
