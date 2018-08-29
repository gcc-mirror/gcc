// PR c++/83186
// { dg-do compile { target c++11 } }

struct a {
  operator unsigned();
};
template <class> void b() { static_cast<unsigned>(a{}); }

int main()
{
  b<int>();
}
