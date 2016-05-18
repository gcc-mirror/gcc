// PR c++/70494
// { dg-do compile { target c++11 } }

struct A { ~A(); };

int main()
{
  A v[] = { A(), A() };
  auto lambda = [v]{};
}
