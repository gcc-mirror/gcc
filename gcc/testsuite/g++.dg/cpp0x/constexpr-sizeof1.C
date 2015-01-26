// PR c++/64603
// { dg-do compile { target c++11 } }

template <int i> constexpr int find_longest_name()
{
  return sizeof("Main") - 1;
}

template <int i, int l = find_longest_name<i>()> void create_all_loggers()
{}

int main()
{
  create_all_loggers<1>();
}
