// PR c++/49776
// { dg-options -std=c++11 }

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
