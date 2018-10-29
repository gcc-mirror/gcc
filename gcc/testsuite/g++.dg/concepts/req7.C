// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

#include <vector>

using namespace std;

template<typename T>
  struct Sequence : std::false_type { };

template<typename T>
  struct Predicate : std::false_type { };

template<typename Seq, typename Fn>
  requires Sequence<Seq>{} and Predicate<Fn>{}
    bool all(const Seq& seq, Fn fn) {
      for(const auto& x : seq)
        if (not fn(x))
          return false;
      return true;
    }

int main() {
  all(vector<int>{0, 2}, true); // { dg-error "not|bool" }
}
