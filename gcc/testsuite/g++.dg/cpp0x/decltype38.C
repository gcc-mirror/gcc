// PR c++/53498
// { dg-do compile { target c++11 } }

template<typename... Args>
struct B
{
  template<typename U>
  static
  void b(const U& u, const Args&... args,
	 decltype(u.f(args...)) dummy)
  {
  }
};

int main() {
  B<int> b;
}
