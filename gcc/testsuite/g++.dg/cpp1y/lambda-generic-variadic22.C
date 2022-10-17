// PR c++/104624
// { dg-do compile { target c++14 } }

template <typename T>
auto f (T)
{
  auto a = [](auto ... i)	// { dg-prune-output "incomplete" }
  {
    int x[][i] = { 0 };		// { dg-error "not expanded" }
  }();
}
void g ()
{
  f(0);
}
