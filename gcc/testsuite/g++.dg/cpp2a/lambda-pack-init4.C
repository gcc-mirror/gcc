// P2095R0
// { dg-do compile { target c++2a } }
// { dg-options "" }

template <class... T>
void f(T... t)
{
  [&...x=t]{};
  [...&x=t]{};			// { dg-warning "7:&" }
}
