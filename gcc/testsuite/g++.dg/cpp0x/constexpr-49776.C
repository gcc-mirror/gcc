// PR c++/49776
// { dg-options -std=c++0x }

struct s
{
  int i[1];

  template<class... Types>
    constexpr s(Types... args)
    : i{args...}  // { dg-error "cannot convert" }
    { }
};

int main()
{
  s test = nullptr;
}
