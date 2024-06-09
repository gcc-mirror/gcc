// PR c++/104867
// { dg-do compile { target c++17 } }

enum class Foo { A1 };

enum class Bar { B1 };

template<auto EnumVal> struct enum_;

template<class...> struct list { };

template<class V> void f(list<enum_<Bar::B1>, V>);

struct enum_type_map : list<enum_<Foo::A1>, int>, list<enum_<Bar::B1>, double> { };

int main() {
  f(enum_type_map());
}
