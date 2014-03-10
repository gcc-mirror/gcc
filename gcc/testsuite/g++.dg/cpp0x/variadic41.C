// A function parameter pack is only deduced if it's at the end
// { dg-do compile { target c++11 } }
template<typename... Args>
void f(const Args&... args, int oops);

int main()
{
  f<>(1);
  f(1);
  f<int>(1,2);
  f(1,2);			// { dg-error "no match" }
}

// { dg-prune-output "note" }
