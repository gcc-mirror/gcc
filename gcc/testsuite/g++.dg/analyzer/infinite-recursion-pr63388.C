// { dg-do compile { target c++11 } }

namespace std
{
  class ostream;
  extern ostream cout;
}

enum class Month {jan=1, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec};

std::ostream& operator<<(std::ostream& os, Month m)
{
  return os << m; // { dg-warning "infinite recursion" }
}

int main()
{
  Month m = Month::may;
  std::cout << m;
  return 0;
}
