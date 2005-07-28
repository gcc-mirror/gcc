// PR c++/16002

void f()
{
  double Q *= 5.0; // { dg-error "initializer" }
}

