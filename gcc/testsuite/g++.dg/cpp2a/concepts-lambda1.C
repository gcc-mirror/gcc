// PR c++/82565
// { dg-do compile { target c++2a } }

struct string
{
  string();
  string(const char *);
  bool empty() const;
};

template<class From, class To>
concept convertible_to = requires(From (&f)(), void (&g)(To)) { g(f()); };

template<typename T, typename ReturnType>
concept Concept =
  requires(T t, const string& s) {
    { t(s) } -> convertible_to<ReturnType>;
  };

struct test {
  string _str;

  template<typename Visitor>
    requires Concept<Visitor, bool>
  decltype(auto) visit(Visitor&& visitor) const {
    return visitor(_str);
  }

};

int main() {
  test().visit([] (auto& x) { return x.empty(); });
}
