// PR c++/82565
// { dg-do compile { target c++14 } }
// { dg-additional-options -fconcepts }

struct string
{
  string();
  string(const char *);
  bool empty() const;
};

template<typename T, typename ReturnType>
concept bool Concept() {
  return requires(T t, const string& s) {
    { t(s) } -> ReturnType;
  };
}

struct test {
  string _str;

  template<typename Visitor>
    requires Concept<Visitor, bool>()
  decltype(auto) visit(Visitor&& visitor) const {
    return visitor(_str);
  }

};

int main() {
  test().visit([] (auto& x) { return x.empty(); });
}
