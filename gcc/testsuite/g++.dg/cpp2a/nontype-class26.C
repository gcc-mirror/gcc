// { dg-do compile { target c++20 } }

struct p { unsigned p_ {}; };

template <p i> struct pp {};
struct qq : public pp <p {}> {};

template <p i> int f (pp <i> const &);

int main ()
{
  return f (qq {});
}
