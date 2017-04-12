// PR c++/78282
// { dg-do compile { target c++14 } }

struct null_node
{
  null_node(const null_node&);
};

extern null_node null;

template <typename T>
auto get() { return null; }

template <typename... Ts>
struct inheritor: Ts...
{
  inheritor(const inheritor& outer)
    : Ts(get<Ts...>())...
    { }
};

void test()
{
  extern inheritor<null_node> example;
  inheritor<null_node> result(example);
}
