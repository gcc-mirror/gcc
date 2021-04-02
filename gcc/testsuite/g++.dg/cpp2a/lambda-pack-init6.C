// PR c++/97938
// { dg-do compile { target c++20 } }

template <typename... Args>
int sink(Args&&... args) { return 2; }

auto fwd1(const auto&&... ts1) {
  return
    [...ts1 = ts1] {
      return sink(ts1...);
    }();
}

template <typename T1>
auto fwd2(const T1& t1) {
  return
    [] (auto&&... ts1) {
      return
	[...ts1 = ts1] {
	  return sink(ts1...);
	}();
    }();
}

int main() {
  return fwd1() + fwd2(1);
}
